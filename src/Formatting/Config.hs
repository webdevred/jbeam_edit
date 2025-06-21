module Formatting.Config (readFormattingConfig) where

import Control.Monad (when)
import Data.Functor (($>))
import Formatting.Rules
import GHC.IO.Exception (IOErrorType (NoSuchThing))
import IOUtils
import Parsing.DSL (parseDSL)
import Paths_jbeam_edit
import System.Directory
import System.FilePath ((</>))

import Data.ByteString.Lazy qualified as BL (
  toStrict,
 )
import Data.Text.IO qualified as TIO (putStrLn)

data ConfigType = MinimalConfig | ComplexConfig deriving (Show)

getJbflSourcePath :: ConfigType -> FilePath
getJbflSourcePath MinimalConfig = "examples" </> "jbfl" </> "minimal.jbfl"
getJbflSourcePath ComplexConfig = "examples" </> "jbfl" </> "complex.jbfl"

getConfigDir :: IO FilePath
getConfigDir = getXdgDirectory XdgConfig "jbeam_edit"

copyConfigFile :: FilePath -> ConfigType -> IO ()
copyConfigFile destDir configType = do
  createDirectoryIfMissing True destDir
  source <- getDataFileName (getJbflSourcePath configType)
  copyFile source (destDir </> "rules.jbfl")

appendRuleFilename :: FilePath -> FilePath
appendRuleFilename configDir = configDir </> "rules.jbfl"

createRuleFileIfDoesNotExist :: FilePath -> IO ()
createRuleFileIfDoesNotExist configDir =
  doesFileExist (appendRuleFilename configDir)
    >>= (`when` copyConfigFile configDir MinimalConfig) . not

readFormattingConfig :: IO RuleSet
readFormattingConfig = do
  configDir <- getConfigDir
  createRuleFileIfDoesNotExist configDir
  contents <- tryReadFile [NoSuchThing] (appendRuleFilename configDir)
  case contents >>= parseDSL . BL.toStrict of
    Right rs -> pure rs
    Left err -> TIO.putStrLn err $> newRuleSet
