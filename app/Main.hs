{-# LANGUAGE CPP #-}

module Main (
  main,
) where

import CommandLineOptions
import Core.Node (Node)
import Formatting (RuleSet, formatNode)
import Formatting.Config
import IOUtils
import Parsing.Jbeam (parseNodes)
import System.Directory (copyFile)

#ifdef ENABLE_TRANSFORMATION
import Transformation (transform)
import Config (loadTransformationConfig)
#endif

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
      case contents >>= parseNodes . toStrict of
        Right ns -> processNodes opts outFilename ns formattingConfig
        Left err -> putTextLn err
    Nothing -> putTextLn "missing arg filename"

processNodes :: Options -> FilePath -> Node -> RuleSet -> IO ()
processNodes opts outFile nodes formattingConfig = do
  transformedNode <- applyTransform opts nodes
  case transformedNode of
    Right transformedNode' ->
      writeFileLBS outFile
        . encodeUtf8
        . toLText
        . formatNode formattingConfig
        $ transformedNode'
    Left err -> putTextLn err

applyTransform :: Options -> Node -> IO (Either Text Node)
#ifdef ENABLE_TRANSFORMATION
applyTransform opts node = do
  tfConfig <- loadTransformationConfig
  pure (transform (optUpdateNames opts) tfConfig node)
#else
applyTransform _ = pure . Right
#endif
