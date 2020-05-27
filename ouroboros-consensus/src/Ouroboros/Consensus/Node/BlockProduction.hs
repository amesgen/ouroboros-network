{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Ouroboros.Consensus.Node.BlockProduction (
    BlockProduction(..)
  , blockProductionIO
  , blockProductionIOLike
  ) where

import           Control.Monad.Trans (lift)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Random

-- | Stateful wrapper around block production
data BlockProduction m blk = BlockProduction {
      -- | Check if we should produce a block
      getLeaderProof :: Ticked (LedgerView (BlockProtocol blk))
                     -> ConsensusState     (BlockProtocol blk)
                     -> m (Maybe (IsLeader (BlockProtocol blk)))

      -- | Produce a block
      --
      -- The function is passed the contents of the mempool; this is a set of
      -- transactions that is guaranteed to be consistent with the ledger state
      -- (also provided as an argument) and with each other (when applied in
      -- order). In principle /all/ of them could be included in the block (up
      -- to maximum block size).
      --
      -- Note that this function is not run in @m@, but in some monad @n@
      -- which only has the ability to produce random number and access to the
      -- 'ForgeState'.
    , produceBlock :: Update m (ForgeState blk)
                   -> BlockNo               -- Current block number
                   -> TickedLedgerState blk -- Current ledger state
                   -> [GenTx blk]           -- Contents of the mempool
                   -> IsLeader (BlockProtocol blk) -- Proof we are leader
                   -> m blk
    }

blockProductionIO :: (BlockSupportsProtocol blk, CanForge blk)
                  => TopLevelConfig blk -> IO (BlockProduction IO blk)
blockProductionIO cfg = do
    return $ BlockProduction {
        getLeaderProof = defaultGetLeaderProof cfg
      , produceBlock   = forgeBlock cfg
      }

-- | Block production in 'IOLike'
--
-- Unlike 'IO', 'IOLike' does not give us 'MonadRandom', and so we need to
-- simulate it.
blockProductionIOLike :: (IOLike m, BlockSupportsProtocol blk)
                      => TopLevelConfig blk
                      -> StrictTVar m ChaChaDRG
                      -> (   Update (ChaChaT m) (ForgeState blk)
                          -> BlockNo
                          -> TickedLedgerState blk
                          -> [GenTx blk]
                          -> IsLeader (BlockProtocol blk)
                          -> ChaChaT m blk)
                      -> m (BlockProduction m blk)
blockProductionIOLike cfg varRNG forge = do
    return $ BlockProduction {
        getLeaderProof = \ledgerState consensusState ->
          simMonadRandom varRNG $
            defaultGetLeaderProof cfg ledgerState consensusState
      , produceBlock   = \upd bno st txs proof ->
          simMonadRandom varRNG $
            forge (hoistUpdate lift upd) bno st txs proof
      }

{-------------------------------------------------------------------------------
  Get leader proof
-------------------------------------------------------------------------------}

defaultGetLeaderProof :: ( MonadRandom m
                         , ConsensusProtocol (BlockProtocol blk)
                         )
                      => TopLevelConfig blk
                      -> Ticked (LedgerView (BlockProtocol blk))
                      -> ConsensusState     (BlockProtocol blk)
                      -> m (Maybe (IsLeader (BlockProtocol blk)))
defaultGetLeaderProof cfg = checkIsLeader (configConsensus cfg)
