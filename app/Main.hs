{-# LANGUAGE CPP #-}

module Main (
  main,
) where

import CommandLineOptions
import Core.Node (Node)
import Core.NodeCursor (newCursor)
import Formatting (RuleSet, formatNode)
import Formatting.Config
import IOUtils
import Parsing.Jbeam (parseNodes)
import System.Directory (copyFile)

import Core.NodeCursor qualified as NC

#ifdef ENABLE_TRANSFORMATION
import Transformation (transform)
#endif

import Data.ByteString.Lazy qualified as BL (
  toStrict,
 )

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
        Left err -> putTextLn err
    Nothing -> putTextLn "missing arg filename"

processNodes :: FilePath -> Node -> RuleSet -> IO ()
processNodes outFile nodes formattingConfig =
  case applyTransform newCursor nodes of
    Right transformedNode ->
      writeFileLBS outFile
        . encodeUtf8
        . toLText
        . formatNode formattingConfig newCursor
        $ transformedNode
    Left err -> putTextLn err

applyTransform :: NC.NodeCursor -> Node -> Either Text Node
#ifdef ENABLE_TRANSFORMATION
applyTransform = transform
#else
applyTransform _ = Right
#endif
