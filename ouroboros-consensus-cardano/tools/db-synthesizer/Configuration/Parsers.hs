

module Configuration.Parsers (
    ForgeOptions (..)
  , NodeCredentials (..)
  , NodeFilePaths (..)
  , parserCommandLine
  ) where

import           Options.Applicative


data NodeFilePaths =
    NodeFilePaths {
        nfpConfig  :: !FilePath
      , nfpChainDB :: !FilePath
    }
    deriving Show

data NodeCredentials =
    NodeCredentials {
        credCertFile :: !FilePath
      , credVRFFile  :: !FilePath
      , credKESFile  :: !FilePath
    }
    deriving Show

newtype ForgeOptions =
    ForgeOptions {
      foptSlotCount :: Int
    }
    deriving Show


parserCommandLine :: Parser (NodeFilePaths, NodeCredentials, ForgeOptions)
parserCommandLine =
  (,,)
    <$> parseNodeFilePaths
    <*> parseNodeCredentials
    <*> parseForgeOptions

parseNodeFilePaths :: Parser NodeFilePaths
parseNodeFilePaths =
  NodeFilePaths
    <$> parseNodeConfigFilePath
    <*> parseChainDBFilePath

parseNodeCredentials :: Parser NodeCredentials
parseNodeCredentials =
  NodeCredentials
    <$> parseOperationalCertFilePath
    <*> parseVrfKeyFilePath
    <*> parseKesKeyFilePath

parseForgeOptions :: Parser ForgeOptions
parseForgeOptions =
  ForgeOptions
    <$> parseSlotCount

parseChainDBFilePath :: Parser FilePath
parseChainDBFilePath =
  strOption
    ( long "db"
        <> metavar "PATH"
        <> help "Path to the Chain DB"
        <> completer (bashCompleter "directory")
    )

parseNodeConfigFilePath :: Parser FilePath
parseNodeConfigFilePath =
  strOption
    ( long "node-config"
        <> metavar "FILE"
        <> help "Path to the node's config.json"
        <> completer (bashCompleter "file")
    )

parseOperationalCertFilePath :: Parser FilePath
parseOperationalCertFilePath =
  strOption
    ( long "shelley-operational-certificate"
        <> metavar "FILE"
        <> help "Path to the delegation certificate"
        <> completer (bashCompleter "file")
    )

parseKesKeyFilePath :: Parser FilePath
parseKesKeyFilePath =
  strOption
    ( long "shelley-kes-key"
        <> metavar "FILE"
        <> help "Path to the KES signing key"
        <> completer (bashCompleter "file")
    )

parseVrfKeyFilePath :: Parser FilePath
parseVrfKeyFilePath =
  strOption
    ( long "shelley-vrf-key"
        <> metavar "FILE"
        <> help "Path to the VRF signing key"
        <> completer (bashCompleter "file")
    )

parseSlotCount :: Parser Int
parseSlotCount =
  option auto
    (     short 's'
       <> long "slots"
       <> metavar "NUMBER"
       <> help "Amount of slots to process"
       <> value 8192        -- arbitrary default number
    )
