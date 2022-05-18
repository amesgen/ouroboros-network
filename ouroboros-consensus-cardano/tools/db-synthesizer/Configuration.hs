{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Configuration (
    module Conf
  , NodeConfigStub (..)
  , parseCommandLine
  ) where


import           Configuration.Parsers as Conf hiding (parserCommandLine)
import qualified Configuration.Parsers as CONF (parserCommandLine)

import           Cardano.Node.Types (AdjustFilePaths (..),
                     NodeByronProtocolConfiguration (..),
                     NodeHardForkProtocolConfiguration (..))

import qualified Cardano.Chain.Update as Byron (ApplicationName (..))
import           Cardano.Crypto (RequiresNetworkMagic (..))

import           Data.Aeson as Aeson (FromJSON (..), Value, withObject, (.!=),
                     (.:), (.:?))

import           Control.Monad (when)
import           Options.Applicative as Opt


data NodeConfigStub =
    NodeConfigStub {
        ncsNodeConfig         :: !Aeson.Value
      , ncsAlonzoGenesisFile  :: !FilePath
      , ncsShelleyGenesisFile :: !FilePath
      , ncsByronGenesisFile   :: !FilePath
    }
    deriving Show

instance FromJSON NodeConfigStub where
    parseJSON val = withObject "NodeConfigStub" (parse' val) val
      where
        parse' o v = do
          proto <- v .: "Protocol"
          when (proto /= ("Cardano" :: String)) $
            fail $ "nodeConfig.Protocol expected: Cardano; found: " ++ proto
          NodeConfigStub o
            <$> v .: "AlonzoGenesisFile"
            <*> v .: "ShelleyGenesisFile"
            <*> v .: "ByronGenesisFile"

{-
instance FromJSON NodeCredentials where
    parseJSON = withObject "NodeCredentials" $ \v ->
        NodeCredentials
          <$> v .: "operationalCertificate"
          <*> v .: "vrfKey"
          <*> v .: "kesKey"
-}

instance AdjustFilePaths NodeConfigStub where
    adjustFilePaths f nc@NodeConfigStub{..} =
        nc {
            ncsAlonzoGenesisFile    = f ncsAlonzoGenesisFile
          , ncsShelleyGenesisFile   = f ncsShelleyGenesisFile
          , ncsByronGenesisFile     = f ncsByronGenesisFile
          }

instance AdjustFilePaths NodeCredentials where
    adjustFilePaths f nc@NodeCredentials{..} =
        nc {
            credCertFile  = f credCertFile
          , credVRFFile   = f credVRFFile
          , credKESFile   = f credKESFile
          }

-- DUPLICATE: mirroring parsers from cardano-node/src/Cardano/Node/Configuration/POM.hs

instance FromJSON NodeHardForkProtocolConfiguration where
    parseJSON = withObject "NodeHardForkProtocolConfiguration" $ \v ->
        NodeHardForkProtocolConfiguration
          <$> v .:? "TestEnableDevelopmentHardForkEras"
                .!= False
          <*> v .:? "TestShelleyHardForkAtEpoch"
          <*> v .:? "TestShelleyHardForkAtVersion"
          <*> v .:? "TestAllegraHardForkAtEpoch"
          <*> v .:? "TestAllegraHardForkAtVersion"
          <*> v .:? "TestMaryHardForkAtEpoch"
          <*> v .:? "TestMaryHardForkAtVersion"
          <*> v .:? "TestAlonzoHardForkAtEpoch"
          <*> v .:? "TestAlonzoHardForkAtVersion"

          <*> v .:? "TestBabbageHardForkAtEpoch"
          <*> v .:? "TestBabbageHardForkAtVersion"

instance FromJSON NodeByronProtocolConfiguration where
    parseJSON = withObject "NodeByronProtocolConfiguration" $ \v ->
        NodeByronProtocolConfiguration
          <$> v .: "ByronGenesisFile"
          <*> v .:? "ByronGenesisHash"
          <*> v .:? "RequiresNetworkMagic"
                .!= RequiresNoMagic
          <*> v .:? "PBftSignatureThreshold"
          -- <*> <- v .:? "ApplicationName"
          --     .!= Byron.ApplicationName "cardano-sl"
          <*> pure (Byron.ApplicationName "db-synthesizer")     --- TODO ???
          <*> v .:? "ApplicationVersion"
                .!= 1
          <*> v .: "LastKnownBlockVersion-Major"
          <*> v .: "LastKnownBlockVersion-Minor"
          <*> v .: "LastKnownBlockVersion-Alt"
                .!= 0

parseCommandLine :: IO (NodeFilePaths, NodeCredentials, ForgeOptions)
parseCommandLine =
    Opt.customExecParser p opts
  where
    p     = Opt.prefs Opt.showHelpOnEmpty
    opts  = Opt.info CONF.parserCommandLine mempty
