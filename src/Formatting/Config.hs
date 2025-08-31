{-# LANGUAGE CPP #-}

module Formatting.Config (readFormattingConfig, copyToConfigDir, ConfigType (..)) where

import Data.ByteString.Lazy as BL
import Formatting.Rules
import GHC.IO.Exception (IOErrorType (NoSuchThing))
import IOUtils
import Parsing.DSL (parseDSL)

#if WINDOWS_EXAMPLE_PATHS
import System.Environment (getExecutablePath)
#else
import Paths_jbeam_edit
#endif

import System.Directory
import System.FilePath (takeDirectory, (</>))

data ConfigType = MinimalConfig | ComplexConfig deriving (Show)

getRelativeJbflSourcePath :: ConfigType -> FilePath
getRelativeJbflSourcePath MinimalConfig = "examples" </> "jbfl" </> "minimal.jbfl"
getRelativeJbflSourcePath ComplexConfig = "examples" </> "jbfl" </> "complex.jbfl"

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

#if WINDOWS_EXAMPLE_PATHS
getJbflSourcePath :: ConfigType -> IO FilePath
getJbflSourcePath configType = do
    executableDir <- takeDirectory <$> getExecutablePath
    pure (executableDir </> getRelativeJbflSourcePath configType)
#else
getJbflSourcePath :: ConfigType -> IO FilePath
getJbflSourcePath configType = getDataFileName (getRelativeJbflSourcePath configType)
#endif

copyConfigFile :: FilePath -> ConfigType -> IO ()
copyConfigFile dest configType = do
  createDirectoryIfMissing True (takeDirectory dest)
  source <- getJbflSourcePath configType
  putStrLn ("installing " ++ show configType ++ " config file to " ++ dest)
  copyFile source dest

copyToConfigDir :: ConfigType -> IO ()
copyToConfigDir configType = do
  configDir <- getConfigDir
  copyConfigFile (configDir </> "rules.jbfl") configType

createRuleFileIfDoesNotExist :: FilePath -> IO ()
createRuleFileIfDoesNotExist configPath =
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
    Left err -> putTextLn err $> newRuleSet
