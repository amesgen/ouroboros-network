{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.Query (
    BlockQuery (.., GetUTxO, GetFilteredUTxO)
  , NonMyopicMemberRewards (..)
  , querySupportedVersion
    -- * Serialisation
  , decodeShelleyQuery
  , decodeShelleyResult
  , encodeShelleyQuery
  , encodeShelleyResult
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise, decode, encode)
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           Data.UMap (domRestrictedView)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))

import           Ouroboros.Network.Block (Serialised (..), decodePoint,
                     encodePoint, mkSerialised)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import           Ouroboros.Consensus.Util (ShowProxy (..))

import qualified Cardano.Ledger.Core as LC
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL (RewardAccounts)
import qualified Cardano.Ledger.Shelley.RewardProvenance as SL
                     (RewardProvenance)
import           Cardano.Ledger.UnifiedMap (View (Delegations, Rewards))
import qualified Cardano.Protocol.TPraos.API as SL

import           Ouroboros.Consensus.Protocol.TPraos (TPraosState (..))
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Config
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion
                     (ShelleyNodeToClientVersion (..))

{-------------------------------------------------------------------------------
  QueryLedger
-------------------------------------------------------------------------------}

newtype NonMyopicMemberRewards c = NonMyopicMemberRewards {
      unNonMyopicMemberRewards ::
        Map (Either SL.Coin (SL.Credential 'SL.Staking c))
            (Map (SL.KeyHash 'SL.StakePool c) SL.Coin)
    }
  deriving stock   (Show)
  deriving newtype (Eq)

type Delegations c =
  Map (SL.Credential 'SL.Staking c)
      (SL.KeyHash 'SL.StakePool c)

instance SL.PraosCrypto c => Serialise (NonMyopicMemberRewards c) where
  encode = toCBOR . unNonMyopicMemberRewards
  decode = NonMyopicMemberRewards <$> fromCBOR

data instance BlockQuery (ShelleyBlock era) :: FootprintL -> Type -> Type where
  GetLedgerTip :: BlockQuery (ShelleyBlock era) SmallL (Point (ShelleyBlock era))
  GetEpochNo :: BlockQuery (ShelleyBlock era) SmallL EpochNo
  -- | Calculate the Non-Myopic Pool Member Rewards for a set of
  -- credentials. See 'SL.getNonMyopicMemberRewards'
  GetNonMyopicMemberRewards
    :: Set (Either SL.Coin (SL.Credential 'SL.Staking (EraCrypto era)))
    -> BlockQuery (ShelleyBlock era) SmallL (NonMyopicMemberRewards (EraCrypto era))
  GetCurrentPParams
    :: BlockQuery (ShelleyBlock era) SmallL (LC.PParams era)
  GetProposedPParamsUpdates
    :: BlockQuery (ShelleyBlock era) SmallL (SL.ProposedPPUpdates era)
  -- | This gets the stake distribution, but not in terms of _active_ stake
  -- (which we need for the leader schedule), but rather in terms of _total_
  -- stake, which is relevant for rewards. It is used by the wallet to show
  -- saturation levels to the end user. We should consider refactoring this, to
  -- an endpoint that provides all the information that the wallet wants about
  -- pools, in an extensible fashion.
  GetStakeDistribution
    :: BlockQuery (ShelleyBlock era) SmallL (SL.PoolDistr (EraCrypto era))

  -- | Get a subset of the UTxO, filtered by address. Although this will
  -- typically return a lot less data than 'GetUTxOWhole', it requires a linear
  -- search over the UTxO and so cost O(n) time.
  --
  -- Only 'GetUTxOByTxIn' is efficient in time and space.
  --
  GetUTxOByAddress
    :: Set (SL.Addr (EraCrypto era))
    -> BlockQuery (ShelleyBlock era) WholeL (SL.UTxO era)

  -- | Get the /entire/ UTxO. This is only suitable for debug/testing purposes
  -- because otherwise it is far too much data.
  --
  GetUTxOWhole
    :: BlockQuery (ShelleyBlock era) WholeL (SL.UTxO era)

  -- | Only for debugging purposes, we make no effort to ensure binary
  -- compatibility (cf the comment on 'GetCBOR'). Moreover, it is huge.
  DebugEpochState
    :: BlockQuery (ShelleyBlock era) SmallL (SL.EpochState era)

  -- | Wrap the result of the query using CBOR-in-CBOR.
  --
  -- For example, when a client is running a different version than the server
  -- and it sends a 'DebugEpochState' query, the client's decoder might fail to
  -- deserialise the epoch state as it might have changed between the two
  -- different versions. The client will then disconnect.
  --
  -- By using CBOR-in-CBOR, the client always successfully decodes the outer
  -- CBOR layer (so no disconnect) and can then manually try to decode the
  -- inner result. When the client's decoder is able to decode the inner
  -- result, it has access to the deserialised epoch state. When it fails to
  -- decode it, the client can fall back to pretty printing the actual CBOR,
  -- which is better than no output at all.
  GetCBOR
    :: BlockQuery (ShelleyBlock era) fp result
    -> BlockQuery (ShelleyBlock era) fp (Serialised result)

  GetFilteredDelegationsAndRewardAccounts
    :: Set (SL.Credential 'SL.Staking (EraCrypto era))
    -> BlockQuery (ShelleyBlock era)
                  SmallL
                  (Delegations (EraCrypto era), SL.RewardAccounts (EraCrypto era))

  GetGenesisConfig
    :: BlockQuery (ShelleyBlock era) SmallL (CompactGenesis era)

  -- | Only for debugging purposes, we make no effort to ensure binary
  -- compatibility (cf the comment on 'GetCBOR'). Moreover, it is huge.
  DebugNewEpochState
    :: BlockQuery (ShelleyBlock era) SmallL (SL.NewEpochState era)

  -- | Only for debugging purposes, we make no effort to ensure binary
  -- compatibility (cf the comment on 'GetCBOR').
  DebugChainDepState
    :: BlockQuery (ShelleyBlock era) SmallL (SL.ChainDepState (EraCrypto era))

  GetRewardProvenance
    :: BlockQuery (ShelleyBlock era) SmallL (SL.RewardProvenance (EraCrypto era))

  -- | Get a subset of the UTxO, filtered by transaction input. This is
  -- efficient and costs only O(m * log n) for m inputs and a UTxO of size n.
  --
  GetUTxOByTxIn
    :: Set (SL.TxIn (EraCrypto era))
    -> BlockQuery (ShelleyBlock era) LargeL (SL.UTxO era)

  GetStakePools
    :: BlockQuery (ShelleyBlock era)
                  SmallL
                  (Set (SL.KeyHash 'SL.StakePool (EraCrypto era)))

  GetStakePoolParams
    :: Set (SL.KeyHash 'SL.StakePool (EraCrypto era))
    -> BlockQuery (ShelleyBlock era)
                  SmallL
                  (Map (SL.KeyHash 'SL.StakePool (EraCrypto era))
                       (SL.PoolParams (EraCrypto era)))

  GetRewardInfoPools
    :: BlockQuery (ShelleyBlock era)
                  SmallL
                  (SL.RewardParams,
                    Map (SL.KeyHash 'SL.StakePool (EraCrypto era))
                        (SL.RewardInfoPool))

  -- WARNING: please add new queries to the end of the list and stick to this
  -- order in all other pattern matches on queries. This helps in particular
  -- with the en/decoders, as we want the CBOR tags to be ordered.
  --
  -- WARNING: when adding a new query, a new @ShelleyNodeToClientVersionX@ must
  -- be added. See #2830 for a template on how to do this.
  --
  -- WARNING: never modify an existing query that has been incorporated in a
  -- release of the node, as it will break compatibility with deployed nodes.
  -- Instead, add a new query. To remove the old query, first to stop supporting
  -- it by modifying 'querySupportedVersion' (@< X@) and when the version is no
  -- longer used (because mainnet has hard-forked to a newer version), it can be
  -- removed.

pattern GetUTxO :: BlockQuery (ShelleyBlock era) WholeL (SL.UTxO era)
pattern GetUTxO = GetUTxOWhole
{-# DEPRECATED GetUTxO "Use 'GetUTxOWhole'" #-}

pattern GetFilteredUTxO :: Set (SL.Addr (EraCrypto era))
                        -> BlockQuery (ShelleyBlock era) WholeL (SL.UTxO era)
pattern GetFilteredUTxO x = GetUTxOByAddress x
{-# DEPRECATED GetFilteredUTxO "Use 'GetUTxOByAddress'" #-}

instance Typeable era => ShowProxy (BlockQuery (ShelleyBlock era)) where

instance ShelleyBasedEra era => QueryLedger (ShelleyBlock era) where
  answerBlockQuery cfg query ext =
      case query of
        GetLedgerTip ->
          shelleyLedgerTipPoint lst
        GetEpochNo ->
          SL.nesEL st
        GetNonMyopicMemberRewards creds ->
          NonMyopicMemberRewards $
            SL.getNonMyopicMemberRewards globals st creds
        GetCurrentPParams ->
          getPParams st
        GetProposedPParamsUpdates ->
          getProposedPPUpdates st
        GetStakeDistribution ->
          SL.poolsByTotalStakeFraction globals st
        GetUTxOByAddress addrs ->
          SL.getFilteredUTxO st addrs
        GetUTxOWhole ->
          SL.getUTxO st
        DebugEpochState ->
          getEpochState st
        GetCBOR query' ->
          mkSerialised (encodeShelleyResult query') $
            answerBlockQuery cfg query' ext
        GetFilteredDelegationsAndRewardAccounts creds ->
          getFilteredDelegationsAndRewardAccounts st creds
        GetGenesisConfig ->
          shelleyLedgerCompactGenesis lcfg
        DebugNewEpochState ->
          st
        DebugChainDepState ->
          tpraosStateChainDepState (headerStateChainDep hst)
        GetRewardProvenance ->
          snd $ SL.getRewardProvenance globals st
        GetUTxOByTxIn txins ->
          SL.getUTxOSubset st txins
        GetStakePools ->
          SL.getPools st
        GetStakePoolParams poolids ->
          SL.getPoolParameters st poolids
        GetRewardInfoPools ->
          SL.getRewardInfoPools globals st
    where
      lcfg    = configLedger $ getExtLedgerCfg cfg
      globals = shelleyLedgerGlobals lcfg
      -- NOTE: we are not pattern matching on @ext@ but using the accessors
      -- here. The reason for that is that that pattern match blows up the
      -- compile time (in particular the time spent desugaring, which is when
      -- the compiler looks at pattern matches) to 2m30s! We don't really
      -- understand why, but our guess is that it has to do with the combination
      -- of the strictness of 'ExtLedgerState', the fact that @LedgerState@ is a
      -- data family, and the 'ShelleyBasedEra' constraint.
      lst = ledgerState ext
      hst = headerState ext
      st  = shelleyLedgerState lst

  prepareBlockQuery = \case
      GetCBOR q        -> prepareBlockQuery q
      GetUTxOByTxIn ks -> ShelleyLedgerTables $ ApplyKeysMK $ HD.UtxoKeys ks

  answerWholeBlockQuery = \case
      GetCBOR qry            -> case answerWholeBlockQuery qry of
        IncrementalQueryHandler
          partial
          empty
          comb
          finish ->
          IncrementalQueryHandler
            partial
            empty
            comb
            (mkSerialised (encodeShelleyResult qry) . finish)
      GetUTxOByAddress addrs ->
        IncrementalQueryHandler
          (\st -> SL.getFilteredUTxO (shelleyLedgerState st) addrs)
          emptyUtxo
          combUtxo
          id
      GetUTxOWhole           ->
        IncrementalQueryHandler
          (\st ->
             let ShelleyLedgerTables (ApplyValuesMK (HD.UtxoValues vs)) = projectLedgerTables st
             in SL.UTxO vs
          )
          emptyUtxo
          combUtxo
          id
    where
      emptyUtxo                        = SL.UTxO Map.empty
      combUtxo (SL.UTxO l) (SL.UTxO r) = SL.UTxO $ Map.union l r

instance EqQuery (BlockQuery (ShelleyBlock era)) where
  eqQuery GetLedgerTip GetLedgerTip
    = Just Refl
  eqQuery GetLedgerTip _
    = Nothing
  eqQuery GetEpochNo GetEpochNo
    = Just Refl
  eqQuery GetEpochNo _
    = Nothing
  eqQuery (GetNonMyopicMemberRewards creds) (GetNonMyopicMemberRewards creds')
    | creds == creds'
    = Just Refl
    | otherwise
    = Nothing
  eqQuery (GetNonMyopicMemberRewards _) _
    = Nothing
  eqQuery GetCurrentPParams GetCurrentPParams
    = Just Refl
  eqQuery GetCurrentPParams _
    = Nothing
  eqQuery GetProposedPParamsUpdates GetProposedPParamsUpdates
    = Just Refl
  eqQuery GetProposedPParamsUpdates _
    = Nothing
  eqQuery GetStakeDistribution GetStakeDistribution
    = Just Refl
  eqQuery GetStakeDistribution _
    = Nothing
  eqQuery (GetUTxOByAddress addrs) (GetUTxOByAddress addrs')
    | addrs == addrs'
    = Just Refl
    | otherwise
    = Nothing
  eqQuery (GetUTxOByAddress _) _
    = Nothing
  eqQuery GetUTxOWhole GetUTxOWhole
    = Just Refl
  eqQuery GetUTxOWhole _
    = Nothing
  eqQuery DebugEpochState DebugEpochState
    = Just Refl
  eqQuery DebugEpochState _
    = Nothing
  eqQuery (GetCBOR q) (GetCBOR q')
    = (\Refl -> Refl) <$> eqQuery q q'
  eqQuery (GetCBOR _) _
    = Nothing
  eqQuery (GetFilteredDelegationsAndRewardAccounts creds)
               (GetFilteredDelegationsAndRewardAccounts creds')
    | creds == creds'
    = Just Refl
    | otherwise
    = Nothing
  eqQuery (GetFilteredDelegationsAndRewardAccounts _) _
    = Nothing
  eqQuery GetGenesisConfig GetGenesisConfig
    = Just Refl
  eqQuery GetGenesisConfig _
    = Nothing
  eqQuery DebugNewEpochState DebugNewEpochState
    = Just Refl
  eqQuery DebugNewEpochState _
    = Nothing
  eqQuery DebugChainDepState DebugChainDepState
    = Just Refl
  eqQuery DebugChainDepState _
    = Nothing
  eqQuery GetRewardProvenance GetRewardProvenance
    = Just Refl
  eqQuery GetRewardProvenance _
    = Nothing
  eqQuery (GetUTxOByTxIn addrs) (GetUTxOByTxIn addrs')
    | addrs == addrs'
    = Just Refl
    | otherwise
    = Nothing
  eqQuery (GetUTxOByTxIn _) _
    = Nothing
  eqQuery GetStakePools GetStakePools
    = Just Refl
  eqQuery GetStakePools _
    = Nothing
  eqQuery (GetStakePoolParams poolids) (GetStakePoolParams poolids')
    | poolids == poolids'
    = Just Refl
    | otherwise
    = Nothing
  eqQuery (GetStakePoolParams _) _
    = Nothing
  eqQuery GetRewardInfoPools GetRewardInfoPools
    = Just Refl
  eqQuery GetRewardInfoPools _
    = Nothing

deriving instance Show (BlockQuery (ShelleyBlock era) fp result)

instance ShelleyBasedEra era => ShowQuery (BlockQuery (ShelleyBlock era)) where
  showResult = \case
      GetLedgerTip                               -> show
      GetEpochNo                                 -> show
      GetNonMyopicMemberRewards {}               -> show
      GetCurrentPParams                          -> show
      GetProposedPParamsUpdates                  -> show
      GetStakeDistribution                       -> show
      GetUTxOByAddress {}                        -> show
      GetUTxOWhole                               -> show
      DebugEpochState                            -> show
      GetCBOR {}                                 -> show
      GetFilteredDelegationsAndRewardAccounts {} -> show
      GetGenesisConfig                           -> show
      DebugNewEpochState                         -> show
      DebugChainDepState                         -> show
      GetRewardProvenance                        -> show
      GetUTxOByTxIn {}                           -> show
      GetStakePools                              -> show
      GetStakePoolParams {}                      -> show
      GetRewardInfoPools                         -> show

instance ShelleyBasedEra era => IsQuery (BlockQuery (ShelleyBlock era)) where
  classifyQuery = \case
      GetLedgerTip                               -> SmallQ
      GetEpochNo                                 -> SmallQ
      GetNonMyopicMemberRewards {}               -> SmallQ
      GetCurrentPParams                          -> SmallQ
      GetProposedPParamsUpdates                  -> SmallQ
      GetStakeDistribution                       -> SmallQ
      GetUTxOByAddress {}                        -> WholeQ
      GetUTxOWhole                               -> WholeQ
      DebugEpochState                            -> SmallQ
      GetCBOR q                                  -> classifyQuery q
      GetFilteredDelegationsAndRewardAccounts {} -> SmallQ
      GetGenesisConfig                           -> SmallQ
      DebugNewEpochState                         -> SmallQ
      DebugChainDepState                         -> SmallQ
      GetRewardProvenance                        -> SmallQ
      GetUTxOByTxIn {}                           -> LargeQ
      GetStakePools                              -> SmallQ
      GetStakePoolParams {}                      -> SmallQ
      GetRewardInfoPools {}                      -> SmallQ

-- | Is the given query supported by the given 'ShelleyNodeToClientVersion'?
querySupportedVersion :: BlockQuery (ShelleyBlock era) fp result -> ShelleyNodeToClientVersion -> Bool
querySupportedVersion = \case
    GetLedgerTip                               -> (>= v1)
    GetEpochNo                                 -> (>= v1)
    GetNonMyopicMemberRewards {}               -> (>= v1)
    GetCurrentPParams                          -> (>= v1)
    GetProposedPParamsUpdates                  -> (>= v1)
    GetStakeDistribution                       -> (>= v1)
    GetUTxOByAddress {}                        -> (>= v1)
    GetUTxOWhole                               -> (>= v1)
    DebugEpochState                            -> (>= v1)
    GetCBOR q                                  -> querySupportedVersion q
    GetFilteredDelegationsAndRewardAccounts {} -> (>= v1)
    GetGenesisConfig                           -> (>= v2)
    DebugNewEpochState                         -> (>= v2)
    DebugChainDepState                         -> (>= v2)
    GetRewardProvenance                        -> (>= v3)
    GetUTxOByTxIn {}                           -> (>= v4)
    GetStakePools                              -> (>= v4)
    GetStakePoolParams {}                      -> (>= v4)
    GetRewardInfoPools                         -> (>= v5)
    -- WARNING: when adding a new query, a new @ShelleyNodeToClientVersionX@
    -- must be added. See #2830 for a template on how to do this.
  where
    v1 = ShelleyNodeToClientVersion1
    v2 = ShelleyNodeToClientVersion2
    v3 = ShelleyNodeToClientVersion3
    v4 = ShelleyNodeToClientVersion4
    v5 = ShelleyNodeToClientVersion5

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getProposedPPUpdates ::
     ShelleyBasedEra era
  => SL.NewEpochState era -> SL.ProposedPPUpdates era
getProposedPPUpdates = SL.proposals . SL._ppups
                     . SL._utxoState . SL.esLState . SL.nesEs

-- Get the current 'EpochState.' This is mainly for debugging.
getEpochState :: SL.NewEpochState era -> SL.EpochState era
getEpochState = SL.nesEs

getDState :: SL.NewEpochState era -> SL.DState (EraCrypto era)
getDState = SL._dstate . SL._delegationState . SL.esLState . SL.nesEs

getFilteredDelegationsAndRewardAccounts ::
     SL.NewEpochState era
  -> Set (SL.Credential 'SL.Staking (EraCrypto era))
  -> (Delegations (EraCrypto era), SL.RewardAccounts (EraCrypto era))
getFilteredDelegationsAndRewardAccounts ss creds =
    (filteredDelegations, filteredRwdAcnts)
  where
    u = SL._unified $ getDState ss

    filteredDelegations = domRestrictedView creds $ Delegations u
    filteredRwdAcnts = domRestrictedView creds $ Rewards u

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeShelleyQuery ::
     ShelleyBasedEra era
  => BlockQuery (ShelleyBlock era) fp result -> Encoding
encodeShelleyQuery query = case query of
    GetLedgerTip ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 0
    GetEpochNo ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 1
    GetNonMyopicMemberRewards creds ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 2 <> toCBOR creds
    GetCurrentPParams ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 3
    GetProposedPParamsUpdates ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 4
    GetStakeDistribution ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 5
    GetUTxOByAddress addrs ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 6 <> toCBOR addrs
    GetUTxOWhole ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 7
    DebugEpochState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 8
    GetCBOR query' ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 9 <> encodeShelleyQuery query'
    GetFilteredDelegationsAndRewardAccounts creds ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 10 <> toCBOR creds
    GetGenesisConfig ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 11
    DebugNewEpochState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 12
    DebugChainDepState ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 13
    GetRewardProvenance ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 14
    GetUTxOByTxIn txins ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 15 <> toCBOR txins
    GetStakePools ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 16
    GetStakePoolParams poolids ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord8 17 <> toCBOR poolids
    GetRewardInfoPools ->
      CBOR.encodeListLen 1 <> CBOR.encodeWord8 18

decodeShelleyQuery ::
     ShelleyBasedEra era
  => Decoder s (SomeQuery (BlockQuery (ShelleyBlock era)))
decodeShelleyQuery = do
    len <- CBOR.decodeListLen
    tag <- CBOR.decodeWord8
    case (len, tag) of
      (1, 0)  -> return $ SomeQuery GetLedgerTip
      (1, 1)  -> return $ SomeQuery GetEpochNo
      (2, 2)  -> SomeQuery . GetNonMyopicMemberRewards <$> fromCBOR
      (1, 3)  -> return $ SomeQuery GetCurrentPParams
      (1, 4)  -> return $ SomeQuery GetProposedPParamsUpdates
      (1, 5)  -> return $ SomeQuery GetStakeDistribution
      (2, 6)  -> SomeQuery . GetUTxOByAddress <$> fromCBOR
      (1, 7)  -> return $ SomeQuery GetUTxOWhole
      (1, 8)  -> return $ SomeQuery DebugEpochState
      (2, 9)  -> (\(SomeQuery q) -> SomeQuery (GetCBOR q)) <$> decodeShelleyQuery
      (2, 10) -> SomeQuery . GetFilteredDelegationsAndRewardAccounts <$> fromCBOR
      (1, 11) -> return $ SomeQuery GetGenesisConfig
      (1, 12) -> return $ SomeQuery DebugNewEpochState
      (1, 13) -> return $ SomeQuery DebugChainDepState
      (1, 14) -> return $ SomeQuery GetRewardProvenance
      (2, 15) -> SomeQuery . GetUTxOByTxIn <$> fromCBOR
      (1, 16) -> return $ SomeQuery GetStakePools
      (2, 17) -> SomeQuery . GetStakePoolParams <$> fromCBOR
      (1, 18) -> return $ SomeQuery GetRewardInfoPools
      _       -> fail $
        "decodeShelleyQuery: invalid (len, tag): (" <>
        show len <> ", " <> show tag <> ")"

encodeShelleyResult ::
     ShelleyBasedEra era
  => BlockQuery (ShelleyBlock era) fp result -> result -> Encoding
encodeShelleyResult query = case query of
    GetLedgerTip                               -> encodePoint encode
    GetEpochNo                                 -> encode
    GetNonMyopicMemberRewards {}               -> encode
    GetCurrentPParams                          -> toCBOR
    GetProposedPParamsUpdates                  -> toCBOR
    GetStakeDistribution                       -> toCBOR
    GetUTxOByAddress {}                        -> toCBOR
    GetUTxOWhole                               -> toCBOR
    DebugEpochState                            -> toCBOR
    GetCBOR {}                                 -> encode
    GetFilteredDelegationsAndRewardAccounts {} -> toCBOR
    GetGenesisConfig                           -> toCBOR
    DebugNewEpochState                         -> toCBOR
    DebugChainDepState                         -> toCBOR
    GetRewardProvenance                        -> toCBOR
    GetUTxOByTxIn {}                           -> toCBOR
    GetStakePools                              -> toCBOR
    GetStakePoolParams {}                      -> toCBOR
    GetRewardInfoPools                         -> toCBOR

decodeShelleyResult ::
     ShelleyBasedEra era
  => BlockQuery (ShelleyBlock era) fp result
  -> forall s. Decoder s result
decodeShelleyResult query = case query of
    GetLedgerTip                               -> decodePoint decode
    GetEpochNo                                 -> decode
    GetNonMyopicMemberRewards {}               -> decode
    GetCurrentPParams                          -> fromCBOR
    GetProposedPParamsUpdates                  -> fromCBOR
    GetStakeDistribution                       -> fromCBOR
    GetUTxOByAddress {}                        -> fromCBOR
    GetUTxOWhole                               -> fromCBOR
    DebugEpochState                            -> fromCBOR
    GetCBOR {}                                 -> decode
    GetFilteredDelegationsAndRewardAccounts {} -> fromCBOR
    GetGenesisConfig                           -> fromCBOR
    DebugNewEpochState                         -> fromCBOR
    DebugChainDepState                         -> fromCBOR
    GetRewardProvenance                        -> fromCBOR
    GetUTxOByTxIn {}                           -> fromCBOR
    GetStakePools                              -> fromCBOR
    GetStakePoolParams {}                      -> fromCBOR
    GetRewardInfoPools                         -> fromCBOR
