module Main
  ( main
  ) where

import qualified Data.ByteString.Lazy as BL (readFile, toStrict, writeFile)
import qualified Data.List as L (uncons)
import qualified Data.Text.IO as TIO (putStrLn)
import qualified Data.Text.Lazy as TL (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import NodeCursor (newCursor)
import System.Environment (getArgs)
import Transformation (transform)

import Formatting
import Parsing

main :: IO ()
main = do
  args <- getArgs
  case L.uncons args of
    Just (filename, _) -> do
      contents <- BL.readFile filename
      let nodes = parseNodes (BL.toStrict contents)
      case nodes of
        Right nodes' ->
          BL.writeFile "hewwu.jbeam"
            . encodeUtf8
            . TL.fromStrict
            . formatNode newCursor
            . transform
            $ nodes'
        Left err -> print err
    Nothing -> TIO.putStrLn "missing arg filename"
