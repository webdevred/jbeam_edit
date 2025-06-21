module Formatting.Config (readFormattingConfig) where

import Data.Functor (($>))
import Formatting.Rules
import GHC.IO.Exception (IOErrorType (NoSuchThing))
import IOUtils
import Parsing.DSL (parseDSL)
import System.FilePath ((</>))

import Data.ByteString.Lazy qualified as BL (
  toStrict,
 )
import Data.Text.IO qualified as TIO (putStrLn)

data ConfigType = MinimalConfig | ComplexConfig deriving (Show)

getJbflSourcePath :: ConfigType -> FilePath
getJbflSourcePath MinimalConfig = "examples" </> "jbfl" </> "minimal.jbfl"
getJbflSourcePath ComplexConfig = "examples" </> "jbfl" </> "minimal.jbfl"


readFormattingConfig :: IO RuleSet
readFormattingConfig = do
  contents <- tryReadFile [NoSuchThing] (getJbflSourcePath MinimalConfig)
  case contents >>= parseDSL . BL.toStrict of
    Right rs -> pure rs
    Left err -> TIO.putStrLn err $> newRuleSet
