{-# LANGUAGE CPP #-}

module JbeamEdit.Formatting.Config (localRuleFile, readFormattingConfig, copyToConfigDir, ConfigType (..)) where

import Data.Foldable (traverse_)
import Data.Text qualified as T
import GHC.IO.Exception (IOErrorType (NoSuchThing))
import JbeamEdit.Formatting.Rules
import JbeamEdit.IOUtils
import JbeamEdit.Parsing.DSL (parseDSL)

#if WINDOWS_EXAMPLE_PATHS
import System.Environment (getExecutablePath)
#else
import Paths_jbeam_edit
#endif

import Control.Monad (unless)
import Data.Functor (($>))
import System.Directory.OsPath
import System.FilePath qualified as FP
import System.OsPath

data ConfigType = MinimalConfig | ComplexConfig deriving (Show)

getRelativeJbflSourcePath :: ConfigType -> FP.FilePath
getRelativeJbflSourcePath MinimalConfig = "examples" FP.</> "jbfl" FP.</> "minimal.jbfl"
getRelativeJbflSourcePath ComplexConfig = "examples" FP.</> "jbfl" FP.</> "complex.jbfl"

getConfigDir :: IO OsPath
getConfigDir = getXdgDirectory XdgConfig (unsafeEncodeUtf "jbeam_edit")

localRuleFile :: OsString
localRuleFile = unsafeEncodeUtf ".jbeam_edit.jbfl"

userRuleFile :: OsString
userRuleFile = unsafeEncodeUtf "rules.jbfl"

getConfigPath :: Maybe OsPath -> OsPath -> IO OsPath
getConfigPath (Just userProvidedPath) _ = pure userProvidedPath
getConfigPath Nothing userConfigDir = do
  projectConfigPath <- fmap (</> localRuleFile) getCurrentDirectory
  projectConfigExists <- doesFileExist projectConfigPath
  if projectConfigExists
    then
      pure projectConfigPath
    else
      pure $ userConfigDir </> userRuleFile

#if WINDOWS_EXAMPLE_PATHS
getJbflSourcePath :: ConfigType -> IO OsPath
getJbflSourcePath configType = do
    executableDir <- FP.takeDirectory <$> getExecutablePath
    pure (unsafeEncodeUtf $ executableDir FP.</> getRelativeJbflSourcePath configType)
#else
getJbflSourcePath :: ConfigType -> IO OsPath
getJbflSourcePath = fmap unsafeEncodeUtf . getDataFileName . getRelativeJbflSourcePath
#endif

copyConfigFile :: OsPath -> ConfigType -> IO ()
copyConfigFile dest configType = do
  createDirectoryIfMissing True (takeDirectory dest)
  source <- getJbflSourcePath configType
  putErrorLine
    ( "installing "
        <> T.show configType
        <> " config file to "
        <> T.show dest
    )
  copyFile source dest

copyToConfigDir :: ConfigType -> IO ()
copyToConfigDir configType = do
  configDir <- getConfigDir
  copyConfigFile (configDir </> userRuleFile) configType

createRuleFileIfDoesNotExist :: OsPath -> IO ()
createRuleFileIfDoesNotExist configPath =
  doesFileExist configPath
    >>= (`unless` copyConfigFile configPath MinimalConfig)

readFormattingConfig :: Maybe OsPath -> IO RuleSet
readFormattingConfig maybeJbflPath = do
  configDir <- getConfigDir
  traverse_ (putErrorLine . (<>) "Loading jbfl: " . T.show) maybeJbflPath
  createRuleFileIfDoesNotExist (configDir </> userRuleFile)
  configPath <- getConfigPath maybeJbflPath configDir
  userCfg <- tryReadFile [NoSuchThing] configPath
  defaultRulesetPath <- getJbflSourcePath MinimalConfig
  defaultCfg <- tryReadFile [] defaultRulesetPath
  case (userCfg >>= parseDSL, defaultCfg >>= parseDSL) of
    (Right rs, Right defaultRs) -> pure (rs <> defaultRs)
    (Left err, Right defaultRs) -> putErrorLine err $> defaultRs
    (_, Left err) ->
      let err' =
            "Failed to parse default ruleset. Please consider making bugreport. \n" <> err
       in putErrorLine err' >> mempty
