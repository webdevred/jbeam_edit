module Main (
  main,
) where

import Control.Exception
import Core.Node (Node)
import Core.NodeCursor (newCursor)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Formatting (RuleSet, formatNode, newRuleSet)
import GHC.IO.Exception (IOErrorType (NoSuchThing), IOException (IOError))
import Parsing.DSL (parseDSL)
import Parsing.Jbeam (parseNodes)
import System.Environment (getArgs)
import Transformation (transform)

import Data.ByteString.Lazy qualified as BL (
  ByteString,
  readFile,
  toStrict,
  writeFile,
 )
import Data.List qualified as L (uncons)
import Data.Text qualified as T (append, pack)
import Data.Text.IO qualified as TIO (putStrLn)
import Data.Text.Lazy qualified as TL (fromStrict)

main :: IO ()
main = do
  args <- getArgs
  formattingConfig <- readFormattingConfig
  case L.uncons args of
    Just (filename, _) -> do
      contents <- tryReadFile [] filename
      case contents >>= parseNodes . BL.toStrict of
        Right ns -> processNodes ns formattingConfig
        Left err -> TIO.putStrLn err
    Nothing -> TIO.putStrLn "missing arg filename"

processNodes :: Node -> RuleSet -> IO ()
processNodes nodes formattingConfig =
  BL.writeFile "hewwu.jbeam"
    . encodeUtf8
    . TL.fromStrict
    . formatNode formattingConfig newCursor
    . transform
    $ nodes

readFormattingConfig :: IO RuleSet
readFormattingConfig = do
  contents <- tryReadFile [NoSuchThing] "rules.jbfl"
  case contents >>= parseDSL . BL.toStrict of
    Right rs -> pure rs
    Left err -> TIO.putStrLn err $> newRuleSet

ioErrorMsg
  :: [IOErrorType]
  -> Either IOException BL.ByteString
  -> Either Text BL.ByteString
ioErrorMsg noerrs (Left (IOError _ ioe_type _ ioe_desc _ filename)) =
  if ioe_type `notElem` noerrs
    then Left $ maybe "" appendColon filename `T.append` T.pack ioe_desc
    else Right ""
  where
    appendColon f = T.pack f `T.append` ": "
ioErrorMsg _ (Right valid) = Right valid

tryReadFile :: [IOErrorType] -> FilePath -> IO (Either Text BL.ByteString)
tryReadFile noerrs fp = do
  possiblyContent <-
    try (BL.readFile fp) :: IO (Either IOException BL.ByteString)
  pure $ ioErrorMsg noerrs possiblyContent
