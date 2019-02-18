{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ouroboros.Storage.VolatileDB.Util where

import           Control.Monad.Catch (ExitCase (..), MonadMask, generalBracket)
import           Control.Monad.Class.MonadSTM
import qualified Data.Text as T
import           Text.Read (readMaybe)

import           Ouroboros.Storage.VolatileDB.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}


parseFd :: String -> Maybe Fd
parseFd = readMaybe
            . T.unpack
            . snd
            . T.breakOnEnd "-"
            . fst
            . T.breakOn "."
            . T.pack

fromEither :: Monad m
           => ErrorHandling e m
           -> Either e a
           -> m a
fromEither err = \case
    Left e -> EH.throwError err e
    Right a -> return a

modifyTMVar :: (MonadSTM m, MonadMask m)
            => TMVar m a
            -> (a -> m (a,b))
            -> m b
modifyTMVar m action =
    snd . fst <$> generalBracket (atomically $ takeTMVar m)
       (\oldState ec -> atomically $ case ec of
            ExitCaseSuccess (newState,_) -> putTMVar m newState
            ExitCaseException _ex        -> putTMVar m oldState
            ExitCaseAbort                -> putTMVar m oldState
       ) action
