{-# LANGUAGE CPP #-}

module Main (
  main,
) where

import CommandLineOptions
import Control.Monad (unless)
import Core.Node (Node)
import Core.NodeCursor (newCursor)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Formatting (RuleSet, formatNode)
import Formatting.Config
import IOUtils
import Parsing.Jbeam (parseNodes)
import System.Directory (copyFile)
import System.Environment (getArgs)

#ifdef ENABLE_TRANSFORMATION
import Transformation (transform)
#endif

import Data.ByteString.Lazy qualified as BL (
  toStrict,
  writeFile,
 )
import Data.Text.IO qualified as TIO (putStrLn)
import Data.Text.Lazy qualified as TL (fromStrict)

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOptions args
  case opts of
    Options {optCopyJbflConfig = Just configType} -> copyToConfigDir configType
    _ -> editFile opts

getWritabaleFilename :: FilePath -> Options -> IO FilePath
getWritabaleFilename filename opts =
  unless (optInPlace opts) (copyFile filename (filename <> ".bak"))
    >> pure filename

editFile :: Options -> IO ()
editFile opts = do
  formattingConfig <- readFormattingConfig
  case optInputFile opts of
    Just filename -> do
      outFilename <- getWritabaleFilename filename opts
      contents <- tryReadFile [] filename
      case contents >>= parseNodes . BL.toStrict of
        Right ns -> processNodes outFilename ns formattingConfig
        Left err -> TIO.putStrLn err
    Nothing -> TIO.putStrLn "missing arg filename"

processNodes :: FilePath -> Node -> RuleSet -> IO ()
processNodes outFile nodes formattingConfig =
  BL.writeFile outFile
    . encodeUtf8
    . TL.fromStrict
    . formatNode formattingConfig newCursor
    $ applyTransform nodes

#ifdef ENABLE_TRANSFORMATION
applyTransform :: Node -> Node
applyTransform = transform
#else
applyTransform :: Node -> Node
applyTransform = id
#endif
