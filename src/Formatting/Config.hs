module Formatting.Config (readFormattingConfig, copyToConfigDir, ConfigType (..)) where

import Control.Monad (when)
import Data.ByteString.Lazy as BL
import Data.Functor (($>))
import Formatting.Rules
import GHC.IO.Exception (IOErrorType (NoSuchThing))
import IOUtils
import Parsing.DSL (parseDSL)
import Paths_jbeam_edit
import System.Directory
import System.FilePath (takeDirectory, (</>))

import Data.Text.IO qualified as TIO (putStrLn)

data ConfigType = MinimalConfig | ComplexConfig deriving (Show)

getJbflSourcePath :: ConfigType -> FilePath
getJbflSourcePath MinimalConfig = "examples" </> "jbfl" </> "minimal.jbfl"
getJbflSourcePath ComplexConfig = "examples" </> "jbfl" </> "complex.jbfl"

getConfigDir :: IO FilePath
getConfigDir = getXdgDirectory XdgConfig "jbeam_edit"

getConfigPath :: FilePath -> IO FilePath
getConfigPath userConfigDir = do
  projectConfigPath <- fmap (</> ".jbeam_edit.jbfl") getCurrentDirectory
  projectConfigExists <- doesFileExist projectConfigPath
  if projectConfigExists
    then
      pure projectConfigPath
    else
      pure $ userConfigDir </> "rules.jbfl"

copyConfigFile :: FilePath -> ConfigType -> IO ()
copyConfigFile dest configType = do
  createDirectoryIfMissing True (takeDirectory dest)
  source <- getDataFileName (getJbflSourcePath configType)
  putStrLn ("installing " ++ show configType ++ " config file to " ++ dest)
  copyFile source dest

copyToConfigDir :: ConfigType -> IO ()
copyToConfigDir configType = do
  configDir <- getConfigDir
  copyConfigFile (configDir </> "rules.jbfl") configType

createRuleFileIfDoesNotExist :: FilePath -> IO ()
createRuleFileIfDoesNotExist configPath = do
  doesFileExist configPath
    >>= (`when` copyConfigFile configPath MinimalConfig) . not

readFormattingConfig :: IO RuleSet
readFormattingConfig = do
  configDir <- getConfigDir
  createRuleFileIfDoesNotExist (configDir </> "rules.jbfl")
  configPath <- getConfigPath configDir
  contents <- tryReadFile [NoSuchThing] configPath
  case contents >>= parseDSL . BL.toStrict of
    Right rs -> pure rs
    Left err -> TIO.putStrLn err $> newRuleSet
