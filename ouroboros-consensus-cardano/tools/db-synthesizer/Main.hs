{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This tool aims to synthesize a valid ChainDB that can be used during node / cluster startup.
module Main (main) where


import           Cardano.Node.Protocol
import           Configuration
import           Forging

import           Cardano.Api.Any (displayError)
import           Cardano.Api.Protocol.Types (protocolInfo)
import           Cardano.Node.Types

-- import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis)
-- import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
-- import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
-- import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis, validateGenesis)

import           Ouroboros.Consensus.Config (configSecurityParam, configStorage)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))

import qualified Ouroboros.Consensus.Node as Node (mkChainDbArgs,
                     stdMkChainDbHasFS)
import qualified Ouroboros.Consensus.Node.InitStorage as Node
                     (nodeImmutableDbChunkInfo)

import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture (dontCheck)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB (defaultArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB (withDB)
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (SnapshotInterval (..), defaultDiskPolicy)
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Data.Aeson as Aeson (FromJSON, Result (..), Value,
                     eitherDecodeStrict', fromJSON)
import           Data.ByteString as BS (ByteString, readFile)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT,
                     handleIOExceptT, hoistEither, runExceptT)

import           Control.Monad (when)
import           Data.Bool (bool)
import           Data.List (sort)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                     listDirectory, makeAbsolute, removePathForcibly)
import           System.Exit
import           System.FilePath (takeDirectory, (</>))


data AppEnv =
    AppEnv {
         envConfigStub          :: NodeConfigStub
       , envForgeOptions        :: ForgeOptions
       , envProtocolCredentials :: ProtocolFilepaths
       -- , envAlonzoGenesis       :: AlonzoGenesis
       -- , envShelleyGenesis      :: ShelleyGenesis StandardShelley
       , envDbDir               :: FilePath
     }

main :: IO ()
main = do
    (NodeFilePaths{..}, NodeCredentials{..}, forgeOptions) <- parseCommandLine

    relativeToConfig :: (FilePath -> FilePath) <-
        (</>) . takeDirectory <$> makeAbsolute nfpConfig

    appEnv_ <- runExceptT $ do
        inp  <- handleIOExceptT show (BS.readFile nfpConfig)
        conf <- adjustFilePaths relativeToConfig <$> readJson inp
        -- alon <- readFileJson $ ncsAlonzoGenesisFile conf
        -- shel <- readFileJson $ ncsShelleyGenesisFile conf
        -- _ <- hoistEither $ validateGenesis shel
        let creds = ProtocolFilepaths {
              byronCertFile         = Nothing
            , byronKeyFile          = Nothing
            , shelleyKESFile        = Just credKESFile
            , shelleyVRFFile        = Just credVRFFile
            , shelleyCertFile       = Just credCertFile
            , shelleyBulkCredsFile  = Nothing
            }
        pure $ AppEnv conf forgeOptions creds nfpChainDB


    appEnv@AppEnv{..} <- either die pure appEnv_

    putStrLn "--> forger credentials:"
    print envProtocolCredentials
    -- putStrLn "----- AlonzoGenesis -----"
    -- print envAlonzoGenesis
    -- putStrLn "----- ShelleyGenesis -----"
    -- print envShelleyGenesis

    let
        shelleyConfig   = NodeShelleyProtocolConfiguration (GenesisFile $ ncsShelleyGenesisFile envConfigStub) Nothing
        alonzoConfig    = NodeAlonzoProtocolConfiguration (GenesisFile $ ncsAlonzoGenesisFile envConfigStub) Nothing
        hfConfig_       = eitherParseJson $ ncsNodeConfig envConfigStub
        byConfig_       = eitherParseJson $ ncsNodeConfig envConfigStub

    protocol <- runExceptT $ do
        hfConfig :: NodeHardForkProtocolConfiguration <-
            hoistEither hfConfig_
        byronConfig :: NodeByronProtocolConfiguration <-
            adjustFilePaths relativeToConfig <$> hoistEither byConfig_

        let
            cardanoConfig = NodeProtocolConfigurationCardano byronConfig shelleyConfig alonzoConfig hfConfig
        firstExceptT displayError $
            mkConsensusProtocol
                cardanoConfig
                (Just envProtocolCredentials)

    either die (synthesize appEnv) protocol

readJson :: (Monad m, FromJSON a) => ByteString -> ExceptT String m a
readJson = hoistEither . eitherDecodeStrict'

{-
readFileJson :: FromJSON a => FilePath -> ExceptT String IO a
readFileJson f = handleIOExceptT show (eitherDecodeFileStrict' f) >>= hoistEither
-}

eitherParseJson :: FromJSON a => Aeson.Value -> Either String a
eitherParseJson v = case fromJSON v of
    Error err -> Left err
    Success a -> Right a


synthesize :: AppEnv -> SomeConsensusProtocol -> IO ()
synthesize AppEnv{..} (SomeConsensusProtocol _ runP) =
    withRegistry $ \registry -> do
        let
            chunkInfo   = Node.nodeImmutableDbChunkInfo (configStorage pInfoConfig)
            k           = configSecurityParam pInfoConfig
            diskPolicy  = defaultDiskPolicy k DefaultSnapshotInterval           -- TODO try without snapshots / make configurable
            dbArgs      = Node.mkChainDbArgs
                registry InFuture.dontCheck pInfoConfig pInfoInitLedger chunkInfo $
                    ChainDB.defaultArgs (Node.stdMkChainDbHasFS envDbDir) diskPolicy

        forgers <- pInfoBlockForging
        let fCount = length forgers
        putStrLn $ "--> forger count: " ++ show fCount
        when (fCount > 0) $ do
            putStrLn "--> clearing ChainDB on file system"
            clearChainDB envDbDir
            ChainDB.withDB dbArgs $ \chainDB ->
                runForge envForgeOptions chainDB (head forgers) pInfoConfig
        putStrLn "--> done"
  where
    ProtocolInfo
        { pInfoConfig
        , pInfoBlockForging
        , pInfoInitLedger
        } = protocolInfo runP


-- may throw exceptions
clearChainDB :: FilePath -> IO ()
clearChainDB db =
    doesDirectoryExist db >>= bool create clear
  where
    create = createDirectoryIfMissing True db
    clear = do
        ls <- listDirectory db
        if sort ls == ["immutable", "ledger", "volatile"]
            then removePathForcibly db >> create
            else fail $ "clearChainDB: '" ++ db ++ "' is non-empty and does not look like a ChainDB. Aborting."
