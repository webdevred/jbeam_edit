module Main (
  main,
) where

import Core.Node (Node)
import Core.NodeCursor (newCursor)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Formatting (RuleSet, formatNode)
import Formatting.Config
import IOUtils
import Parsing.Jbeam (parseNodes)
import System.Environment (getArgs)
import Transformation (transform)

import Data.ByteString.Lazy qualified as BL (
  toStrict,
  writeFile,
 )
import Data.List qualified as L (uncons)
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
