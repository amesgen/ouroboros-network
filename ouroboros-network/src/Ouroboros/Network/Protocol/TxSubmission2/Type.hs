{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}

-- | The type of the transaction submission protocol.
--
-- This is used to relay transactions between nodes.
--
module Ouroboros.Network.Protocol.TxSubmission2.Type where

import           Data.Kind (Type)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Singletons
import           Data.Word (Word16, Word32)

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Util.ShowProxy

-- | Transactions are typically not big, but in principle in future we could
-- have ones over 64k large.
--
type TxSizeInBytes = Word32

-- | The kind of the transaction-submission protocol, and the types of the
-- states in the protocol state machine.
--
-- We describe this protocol using the label \"client\" for the peer that is
-- submitting transactions, and \"server\" for the one receiving them. The
-- protocol is however pull based, so it is typically the server that has
-- agency in this protocol. This is the opposite of the chain sync and block
-- fetch protocols, but that makes sense because the information flow is also
-- reversed: submitting transactions rather than receiving headers and blocks.
--
-- Because these client\/server labels are somewhat confusing in this case, we
-- sometimes clarify by using the terms inbound and outbound. This refers to
-- whether transactions are flowing towards a peer or away, and thus indicates
-- what role the peer is playing.
--
data TxSubmission2 txid tx where

  -- | Initial protocol message.
  --
  StInit   :: TxSubmission2 txid tx

  -- | The server (inbound side) has agency; it can either terminate, ask for
  -- transaction identifiers or ask for transactions.
  --
  -- There is no timeout in this state.
  --
  StIdle   :: TxSubmission2 txid tx

  -- | The client (outbound side) has agency; it must reply with a
  -- list of transaction identifiers that it wishes to submit.
  --
  -- There are two sub-states for this, for blocking and non-blocking cases.
  --
  StTxIds  :: BlockingStyle -> TxSubmission2 txid tx

  -- | The client (outbound side) has agency; it must reply with the list of
  -- transactions.
  --
  StTxs    :: TxSubmission2 txid tx

  -- | Nobody has agency; termination state.
  --
  StDone   :: TxSubmission2 txid tx

type SingTxSubmission :: TxSubmission2 txid tx
                      -> Type
data SingTxSubmission k where
    SingInit  :: SingTxSubmission  StInit
    SingIdle  :: SingTxSubmission  StIdle
    SingTxIds :: SingBlockingStyle stBlocking
              -> SingTxSubmission (StTxIds stBlocking)
    SingTxs   :: SingTxSubmission  StTxs
    SingDone  :: SingTxSubmission  StDone

deriving instance Show (SingTxSubmission k)

type instance Sing = SingTxSubmission
instance SingI StInit               where sing = SingInit
instance SingI StIdle               where sing = SingIdle
instance SingI stBlocking
      => SingI (StTxIds stBlocking) where sing = SingTxIds sing
instance SingI StTxs                where sing = SingTxs
instance SingI StDone               where sing = SingDone

instance ( ShowProxy txid
         , ShowProxy tx
         ) => ShowProxy (TxSubmission2 txid tx) where
    showProxy _ = concat
      [ "TxSubmission2 "
      , showProxy (Proxy :: Proxy txid)
      , " "
      , showProxy (Proxy :: Proxy tx)
      ]

instance ShowProxy (StIdle :: TxSubmission2 txid tx) where
    showProxy _ = "StIdle"


data BlockingStyle where

  -- | In this sub-state the reply need not be prompt. There is no timeout.
  --
  Blocking    :: BlockingStyle

  -- | In this state the peer must reply. There is a timeout.
  --
  NonBlocking :: BlockingStyle


-- | There are some constraints of the protocol that are not captured in the
-- types of the messages, but are documented with the messages. Violation
-- of these constraints is also a protocol error. The constraints are intended
-- to ensure that implementations are able to work in bounded space.
--
instance Protocol (TxSubmission2 txid tx) where

  -- | The messages in the transaction submission protocol.
  --
  -- In this protocol the consumer (inbound side, server role) always
  -- initiates and the producer (outbound side, client role) replies.
  -- This makes it a pull based protocol where the receiver manages the
  -- control flow.
  --
  -- The protocol involves asking for transaction identifiers, and then
  -- asking for transactions corresponding to the identifiers of interest.
  --
  -- There are two ways to ask for transaction identifiers, blocking and
  -- non-blocking. They otherwise have the same semantics.
  --
  -- The protocol maintains a notional FIFO of \"outstanding\" transaction
  -- identifiers that have been provided but not yet acknowledged. Only
  -- transactions that are outstanding can be requested: they can be
  -- requested in any order, but at most once. Transaction identifiers are
  -- acknowledged in the same FIFO order they were provided in. The
  -- acknowledgement is included in the same messages used to ask for more
  -- transaction identifiers.
  --
  data Message (TxSubmission2 txid tx) from to where

    MsgInit
      :: Message (TxSubmission2 txid tx) StInit StIdle

    -- | Request a non-empty list of transaction identifiers from the client,
    -- and confirm a number of outstanding transaction identifiers.
    --
    -- With 'TokBlocking' this is a a blocking operation: the response will
    -- always have at least one transaction identifier, and it does not expect
    -- a prompt response: there is no timeout. This covers the case when there
    -- is nothing else to do but wait. For example this covers leaf nodes that
    -- rarely, if ever, create and submit a transaction.
    --
    -- With 'TokNonBlocking' this is a non-blocking operation: the response
    -- may be an empty list and this does expect a prompt response. This
    -- covers high throughput use cases where we wish to pipeline, by
    -- interleaving requests for additional transaction identifiers with
    -- requests for transactions, which requires these requests not block.
    --
    -- The request gives the maximum number of transaction identifiers that
    -- can be accepted in the response. This must be greater than zero in the
    -- 'TokBlocking' case. In the 'TokNonBlocking' case either the numbers
    -- acknowledged or the number requested must be non-zero. In either case,
    -- the number requested must not put the total outstanding over the fixed
    -- protocol limit.
    --
    -- The request also gives the number of outstanding transaction
    -- identifiers that can now be acknowledged. The actual transactions
    -- to acknowledge are known to the peer based on the FIFO order in which
    -- they were provided.
    --
    -- There is no choice about when to use the blocking case versus the
    -- non-blocking case, it depends on whether there are any remaining
    -- unacknowledged transactions (after taking into account the ones
    -- acknowledged in this message):
    --
    -- * The blocking case must be used when there are zero remaining
    --   unacknowledged transactions.
    --
    -- * The non-blocking case must be used when there are non-zero remaining
    --   unacknowledged transactions.
    --
    MsgRequestTxIds
      :: SingBlockingStyle blocking
      -> Word16 -- ^ Acknowledge this number of outstanding txids
      -> Word16 -- ^ Request up to this number of txids.
      -> Message (TxSubmission2 txid tx) StIdle (StTxIds blocking)

    -- | Reply with a list of transaction identifiers for available
    -- transactions, along with the size of each transaction.
    --
    -- The list must not be longer than the maximum number requested.
    --
    -- In the 'StTxIds' 'Blocking' state the list must be non-empty while
    -- in the 'StTxIds' 'NonBlocking' state the list may be empty.
    --
    -- These transactions are added to the notional FIFO of outstanding
    -- transaction identifiers for the protocol.
    --
    -- The order in which these transaction identifiers are returned must be
    -- the order in which they are submitted to the mempool, to preserve
    -- dependent transactions.
    --
    MsgReplyTxIds
      :: BlockingReplyList blocking (txid, TxSizeInBytes)
      -> Message (TxSubmission2 txid tx) (StTxIds blocking) StIdle

    -- | Request one or more transactions corresponding to the given
    -- transaction identifiers.
    --
    -- While it is the responsibility of the replying peer to keep within
    -- pipelining in-flight limits, the sender must also cooperate by keeping
    -- the total requested across all in-flight requests within the limits.
    --
    -- It is an error to ask for transaction identifiers that were not
    -- previously announced (via 'MsgReplyTxIds').
    --
    -- It is an error to ask for transaction identifiers that are not
    -- outstanding or that were already asked for.
    --
    MsgRequestTxs
      :: [txid]
      -> Message (TxSubmission2 txid tx) StIdle StTxs

    -- | Reply with the requested transactions, or implicitly discard.
    --
    -- Transactions can become invalid between the time the transaction
    -- identifier was sent and the transaction being requested. Invalid
    -- (including committed) transactions do not need to be sent.
    --
    -- Any transaction identifiers requested but not provided in this reply
    -- should be considered as if this peer had never announced them. (Note
    -- that this is no guarantee that the transaction is invalid, it may still
    -- be valid and available from another peer).
    --
    MsgReplyTxs
      :: [tx]
      -> Message (TxSubmission2 txid tx) StTxs StIdle

    -- | Termination message, initiated by the client when the server is
    -- making a blocking call for more transaction identifiers.
    --
    MsgDone
      :: Message (TxSubmission2 txid tx) (StTxIds Blocking) StDone


  type StateAgency  StInit     = ClientAgency
  type StateAgency (StTxIds b) = ClientAgency
  type StateAgency  StTxs      = ClientAgency
  type StateAgency  StIdle     = ServerAgency
  type StateAgency  StDone     = NobodyAgency



-- | The value level equivalent of 'BlockingStyle'.
--
-- This is also used in 'MsgRequestTxIds' where it is interpreted (and can be
-- encoded) as a 'Bool' with 'True' for blocking, and 'False' for non-blocking.
--
data SingBlockingStyle (k :: BlockingStyle) where
  SingBlocking    :: SingBlockingStyle Blocking
  SingNonBlocking :: SingBlockingStyle NonBlocking

deriving instance Eq   (SingBlockingStyle b)
deriving instance Show (SingBlockingStyle b)
type instance Sing = SingBlockingStyle
instance SingI Blocking    where sing = SingBlocking
instance SingI NonBlocking where sing = SingNonBlocking

-- | We have requests for lists of things. In the blocking case the
-- corresponding reply must be non-empty, whereas in the non-blocking case
-- and empty reply is fine.
--
data BlockingReplyList (blocking :: BlockingStyle) a where
  BlockingReply    :: NonEmpty a  -> BlockingReplyList Blocking    a
  NonBlockingReply ::         [a] -> BlockingReplyList NonBlocking a

deriving instance Eq   a => Eq   (BlockingReplyList blocking a)
deriving instance Show a => Show (BlockingReplyList blocking a)

deriving instance (Eq txid, Eq tx) =>
                  Eq (Message (TxSubmission2 txid tx) from to)

deriving instance (Show txid, Show tx) =>
                  Show (Message (TxSubmission2 txid tx) from to)
