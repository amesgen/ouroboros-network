{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Forging (runForge) where

import           Configuration (ForgeOptions (..))

import           Control.Exception
import           Control.Monad (when)
import           Control.Monad.Except (runExcept)
import           Data.Maybe (isJust)
import           Data.Proxy

import           Ouroboros.Consensus.Block.Abstract as Block
import           Ouroboros.Consensus.Block.Forging as Block (BlockForging (..),
                     ShouldForge (..), checkShouldForge)
import           Ouroboros.Consensus.HeaderValidation
                     (BasicEnvelopeValidation (..), HeaderState (..))

import           Ouroboros.Consensus.Storage.ChainDB.API as ChainDB (ChainDB,
                     addBlockAsync, blockProcessed, getCurrentChain,
                     getPastLedger)
import qualified Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment as InvalidBlockPunishment
                     (noPunishment)

import           Ouroboros.Network.AnchoredFragment as AF (Anchor (..),
                     AnchoredFragment, AnchoredSeq (..), headPoint)

import           Ouroboros.Consensus.Config (TopLevelConfig, configConsensus,
                     configLedger)
import           Ouroboros.Consensus.Forecast (forecastFor)
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState,
                     tickChainDepState)
import           Ouroboros.Consensus.Util.IOLike (atomically)

import           Control.Tracer as Trace (nullTracer)


data ForgeState =
  ForgeState {
    counter     :: !Int
  , currentSlot :: !SlotNo
  , forged      :: !Int
  }

-- just a shim; we don't need to lift into WithEarlyExit monad
lift :: a -> a
lift = id

runForge
  :: forall blk.
    ( LedgerSupportsProtocol blk )
    => ForgeOptions
    -> ChainDB IO blk
    -> BlockForging IO blk
    -> TopLevelConfig blk
    -> IO ()
runForge ForgeOptions{..} chainDB blockForging cfg = do
    putStrLn $ "--> will process " ++ show foptSlotCount ++ " slots"
    ForgeState{forged} <- go $ ForgeState foptSlotCount 0 0
    putStrLn $ "--> forged and adopted a block in " ++ show forged ++ " slots"
  where
    go :: ForgeState -> IO ForgeState
    go forgeState@ForgeState{..}
      | counter == 0 = pure forgeState
      | otherwise =
        let forgeState' = forgeState {counter = counter - 1, currentSlot = succ currentSlot}
        in try (goSlot currentSlot) >>= \case
          Left SomeException{} -> go forgeState'
          _                    -> go forgeState' {forged = forged + 1}

    exitEarly' = fail

    goSlot :: SlotNo -> IO ()
    goSlot currentSlot = do
        -- trace $ TraceStartLeadershipCheck currentSlot

        -- Figure out which block to connect to
        --
        -- Normally this will be the current block at the tip, but it may
        -- be the /previous/ block, if there were multiple slot leaders
        BlockContext{bcBlockNo, bcPrevPoint} <- do
          eBlkCtx <- lift $ atomically $
            mkCurrentBlockContext currentSlot
                <$> ChainDB.getCurrentChain chainDB
          case eBlkCtx of
            Right blkCtx -> return blkCtx
            Left failure -> do
              -- trace failure
              exitEarly' $ "no block context: " ++ show failure

        -- trace $ TraceBlockContext currentSlot bcBlockNo bcPrevPoint

        -- Get ledger state corresponding to bcPrevPoint
        --
        -- This might fail if, in between choosing 'bcPrevPoint' and this call to
        -- 'getPastLedger', we switched to a fork where 'bcPrevPoint' is no longer
        -- on our chain. When that happens, we simply give up on the chance to
        -- produce a block.
        unticked <- do
          mExtLedger <- lift $ atomically $ ChainDB.getPastLedger chainDB bcPrevPoint
          case mExtLedger of
            Just l  -> return l
            Nothing -> do
              -- trace $ TraceNoLedgerState currentSlot bcPrevPoint
              exitEarly' "no ledger state"

        -- trace $ TraceLedgerState currentSlot bcPrevPoint

        -- We require the ticked ledger view in order to construct the ticked
        -- 'ChainDepState'.
        ledgerView <-
          case runExcept $ forecastFor
                           (ledgerViewForecastAt
                              (configLedger cfg)
                              (ledgerState unticked))
                           currentSlot of
            Left err -> do
              -- There are so many empty slots between the tip of our chain and
              -- the current slot that we cannot get an ledger view anymore
              -- In principle, this is no problem; we can still produce a block
              -- (we use the ticked ledger state). However, we probably don't
              -- /want/ to produce a block in this case; we are most likely
              -- missing a blocks on our chain.
              -- trace $ TraceNoLedgerView currentSlot err
              exitEarly' $ "no ledger view: " ++ show err
            Right lv ->
              return lv

        -- trace $ TraceLedgerView currentSlot

        -- Tick the 'ChainDepState' for the 'SlotNo' we're producing a block
        -- for. We only need the ticked 'ChainDepState' to check the whether
        -- we're a leader. This is much cheaper than ticking the entire
        -- 'ExtLedgerState'.
        let tickedChainDepState :: Ticked (ChainDepState (BlockProtocol blk))
            tickedChainDepState =
                tickChainDepState
                  (configConsensus cfg)
                  ledgerView
                  currentSlot
                  (headerStateChainDep (headerState unticked))

        -- Check if we are the leader
        proof <- do
          shouldForge <- lift $
            checkShouldForge blockForging
              -- (contramap (TraceLabelCreds (forgeLabel blockForging))
              --   (forgeStateInfoTracer tracers))
              nullTracer
              cfg
              currentSlot
              tickedChainDepState
          case shouldForge of
            ForgeStateUpdateError _ ->
              exitEarly' "ForgeStateUpdateError"
            CannotForge _ ->
              exitEarly' "CannotForge"
            NotLeader ->
              exitEarly' "NotLeader"
            ShouldForge p -> return p

        -- At this point we have established that we are indeed slot leader
        -- trace $ TraceNodeIsLeader currentSlot

        -- Tick the ledger state for the 'SlotNo' we're producing a block for
        let tickedLedgerState :: Ticked (LedgerState blk)
            tickedLedgerState =
              applyChainTick
                (configLedger cfg)
                currentSlot
                (ledgerState unticked)

        -- Get a snapshot of the mempool that is consistent with the ledger
        --
        -- NOTE: It is possible that due to adoption of new blocks the
        -- /current/ ledger will have changed. This doesn't matter: we will
        -- produce a block that fits onto the ledger we got above; if the
        -- ledger in the meantime changes, the block we produce here may or
        -- may not be adopted, but it won't be invalid.
        {-
        mempoolSnapshot <- lift $ atomically $
                             getSnapshotFor
                               mempool
                               (ForgeInKnownSlot
                                  currentSlot
                                  tickedLedgerState)
        let txs = map fst $ snapshotTxs mempoolSnapshot
        -}

        let txs = []

        -- Actually produce the block
        newBlock <- lift $
          Block.forgeBlock blockForging
            cfg
            bcBlockNo
            currentSlot
            tickedLedgerState
            txs
            proof

        {-
        trace $ TraceForgedBlock
                  currentSlot
                  (ledgerTipPoint (Proxy @blk) (ledgerState unticked))
                  newBlock
                  (snapshotMempoolSize mempoolSnapshot)
        -}

        -- Add the block to the chain DB
        let noPunish = InvalidBlockPunishment.noPunishment   -- no way to punish yourself
        result <- lift $ ChainDB.addBlockAsync chainDB noPunish newBlock
        -- Block until we have processed the block
        curTip <- lift $ atomically $ ChainDB.blockProcessed result

        -- Check whether we adopted our block
        when (curTip /= blockPoint newBlock) $ do
          {-
          isInvalid <- lift $ atomically $
            ($ blockHash newBlock) . forgetFingerprint <$>
            ChainDB.getIsInvalidBlock chainDB
          case isInvalid of
            Nothing ->
              trace $ TraceDidntAdoptBlock currentSlot newBlock
            Just reason -> do
              trace $ TraceForgedInvalidBlock currentSlot newBlock reason
              -- We just produced a block that is invalid according to the
              -- ledger in the ChainDB, while the mempool said it is valid.
              -- There is an inconsistency between the two!
              --
              -- Remove all the transactions in that block, otherwise we'll
              -- run the risk of forging the same invalid block again. This
              -- means that we'll throw away some good transactions in the
              -- process.
              lift $ removeTxs mempool (map (txId . txForgetValidated) txs)
            -}
            exitEarly' "block not adopted"

        -- We successfully produced /and/ adopted a block
        --
        -- NOTE: we are tracing the transactions we retrieved from the Mempool,
        -- not the transactions actually /in the block/. They should always
        -- match, if they don't, that would be a bug. Unfortunately, we can't
        -- assert this here because the ability to extract transactions from a
        -- block, i.e., the @HasTxs@ class, is not implementable by all blocks,
        -- e.g., @DualBlock@.
        -- trace $ TraceAdoptedBlock currentSlot newBlock txs

        -- putStrLn $ "forged and adopted a block in: " ++ show currentSlot


{-
    trace :: TraceForgeEvent blk -> WithEarlyExit m ()
    trace =
          lift
        . traceWith (forgeTracer tracers)
        . TraceLabelCreds (forgeLabel blockForging)
-}

-- | Context required to forge a block
data BlockContext blk = BlockContext
  { bcBlockNo   :: !BlockNo
    -- ^ the block number of the block to be forged
  , bcPrevPoint :: !(Point blk)
    -- ^ the point of /the predecessor of/ the block
    --
    -- Note that a block/header stores the hash of its predecessor but not the
    -- slot.
  }

  -- | Create the 'BlockContext' from the header of the previous block
blockContextFromPrevHeader ::
     HasHeader (Header blk)
  => Header blk -> BlockContext blk
blockContextFromPrevHeader hdr =
    -- Recall that an EBB has the same block number as its predecessor, so this
    -- @succ@ is even correct when @hdr@ is an EBB.
    BlockContext (succ (blockNo hdr)) (headerPoint hdr)

-- | Determine the 'BlockContext' for a block about to be forged from the
-- current slot, ChainDB chain fragment, and ChainDB tip block number
--
-- The 'bcPrevPoint' will either refer to the header at the tip of the current
-- chain or, in case there is already a block in this slot (e.g. another node
-- was also elected leader and managed to produce a block before us), the tip's
-- predecessor. If the chain is empty, then it will refer to the chain's anchor
-- point, which may be genesis.
mkCurrentBlockContext
  :: forall blk. -- RunNode blk
     -- HasHeader (Header blk)
     ( GetHeader blk
     , BasicEnvelopeValidation blk )
  => SlotNo
     -- ^ the current slot, i.e. the slot of the block about to be forged
  -> AnchoredFragment (Header blk)
     -- ^ the current chain fragment
     --
     -- Recall that the anchor point is the tip of the ImmutableDB.
  -> Either () (BlockContext blk)
  -- -> Either (TraceForgeEvent blk) (BlockContext blk)
     -- ^ the event records the cause of the failure
mkCurrentBlockContext currentSlot c = case c of
    Empty AF.AnchorGenesis ->
      -- The chain is entirely empty.
      Right $ BlockContext (expectedFirstBlockNo (Proxy @blk)) GenesisPoint

    Empty (AF.Anchor anchorSlot anchorHash anchorBlockNo) ->
      let p :: Point blk = BlockPoint anchorSlot anchorHash
      in if anchorSlot < currentSlot
           then Right $ BlockContext (succ anchorBlockNo) p
           -- else Left  $ TraceSlotIsImmutable currentSlot p anchorBlockNo
           else Left ()

    c' :> hdr -> case blockSlot hdr `compare` currentSlot of

      -- The block at the tip of our chain has a slot number /before/ the
      -- current slot number. This is the common case, and we just want to
      -- connect our new block to the block at the tip.
      LT -> Right $ blockContextFromPrevHeader hdr

      -- The block at the tip of our chain has a slot that lies in the
      -- future. Although the chain DB does not adopt future blocks, if the
      -- system is under heavy load, it is possible (though unlikely) that
      -- one or more slots have passed after @currentSlot@ that we got from
      -- @onSlotChange@ and and before we queried the chain DB for the block
      -- at its tip. At the moment, we simply don't produce a block if this
      -- happens.

      -- TODO: We may wish to produce a block here anyway, treating this
      -- as similar to the @EQ@ case below, but we should be careful:
      --
      -- 1. We should think about what slot number to use.
      -- 2. We should be careful to distinguish between the case where we
      --    need to drop a block from the chain and where we don't.
      -- 3. We should be careful about slot numbers and EBBs.
      -- 4. We should probably not produce a block if the system is under
      --    very heavy load (e.g., if a lot of blocks have been produced
      --    after @currentTime@).
      --
      -- See <https://github.com/input-output-hk/ouroboros-network/issues/1462>
      -- GT -> Left $ TraceBlockFromFuture currentSlot (blockSlot hdr)
      GT -> Left ()

      -- The block at the tip has the same slot as the block we're going to
      -- produce (@currentSlot@).
      EQ -> Right $ if isJust (headerIsEBB hdr)
        -- We allow forging a block that is the successor of an EBB in the
        -- same slot.
        then blockContextFromPrevHeader hdr
        -- If @hdr@ is not an EBB, then forge an alternative to @hdr@: same
        -- block no and same predecessor.
        else BlockContext (blockNo hdr) $ castPoint $ AF.headPoint c'
