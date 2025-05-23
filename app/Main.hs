module Main
  ( main
  ) where

import Data.Bifunctor (first)
import Core.NodeCursor (newCursor)
import qualified Data.ByteString.Lazy as BL (ByteString,readFile, toStrict, writeFile)
import Data.Functor (($>))
import qualified Data.List as L (uncons)
import qualified Data.Text.IO as TIO (putStrLn)
import qualified Data.Text.Lazy as TL (fromStrict)
import qualified Data.Text as T (pack)
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.Environment (getArgs)
import Control.Exception
import Transformation (transform)

import Formatting
import Parsing.DSL
import Parsing.Jbeam qualified as J

main :: IO ()
main = do
  args <- getArgs
  formattingConfig <- readFormattingConfig
  case L.uncons args of
    Just (filename, _) -> do
      contents <- BL.readFile filename
      let nodes = J.parseNodes (BL.toStrict contents)
      case nodes of
        Right nodes' ->
          BL.writeFile "hewwu.jbeam"
            . encodeUtf8
            . TL.fromStrict
            . formatNode formattingConfig newCursor
            . transform
            $ nodes'
        Left err -> TIO.putStrLn err
    Nothing -> TIO.putStrLn "missing arg filename"


tryReadFile :: FilePath -> IO (Either Text BL.ByteString)
tryReadFile fp = do
  possiblyContent <- try (BL.readFile fp) :: IO (Either IOException BL.ByteString)
  pure $ first (T.pack . show) possiblyContent

readFormattingConfig :: IO RuleSet
readFormattingConfig = do
  contents <- tryReadFile "rules.jbfl"
  case contents >>= parseDSL . BL.toStrict  of
    Right rs -> pure rs
    Left err -> TIO.putStrLn err $> newRuleSet
