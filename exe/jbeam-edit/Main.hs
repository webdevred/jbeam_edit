module Main (
  main,
) where

import CommandLineOptions
import Control.Monad (unless)
import Core.Node (Node)
import Data.ByteString.Lazy qualified as LBS (fromStrict, toStrict, writeFile)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Formatting (RuleSet, formatNode)
import Formatting.Config
import IOUtils
import Parsing.Jbeam (parseNodes)
import System.Directory (copyFile)
import System.Environment (getArgs)

#ifdef ENABLE_WINDOWS_NEWLINES
import Data.Text qualified as T
#endif

import System.FilePath (dropExtension)

#ifdef ENABLE_TRANSFORMATION
import JbeamEdit.Transformation (transform)
import System.FilePath ((</>))
import JbeamEdit.Transformation.Config
import System.Directory (getCurrentDirectory)
#endif

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOptions args
  case opts of
    Options {optCopyJbflConfig = Just configType} -> copyToConfigDir configType
    _ -> editFile opts

getWritabaleFilename :: FilePath -> Options -> IO FilePath
getWritabaleFilename filename opts = do
  let backupFilename = dropExtension filename <> ".bak.jbeam"
  unless
    (optInPlace opts)
    (copyFile filename backupFilename)
  pure filename

editFile :: Options -> IO ()
editFile opts = do
  formattingConfig <- readFormattingConfig
  case optInputFile opts of
    Just filename -> do
      outFilename <- getWritabaleFilename filename opts
      contents <- tryReadFile [] filename
      case contents >>= parseNodes . LBS.toStrict of
        Right ns -> processNodes opts outFilename ns formattingConfig
        Left err -> putErrorLine err
    Nothing -> putErrorLine "missing arg filename"

processNodes :: Options -> FilePath -> Node -> RuleSet -> IO ()
processNodes opts outFile nodes formattingConfig = do
  transformedNode <- applyTransform opts nodes
  case transformedNode of
    Right transformedNode' ->
      LBS.writeFile outFile
        . LBS.fromStrict
        . encodeUtf8
        . replaceNewlines
        . formatNode formattingConfig
        $ transformedNode'
    Left err -> putErrorLine err

#ifdef ENABLE_WINDOWS_NEWLINES
replaceNewlines :: Text -> Text
replaceNewlines = T.replace "\n" "\r\n"
#else
replaceNewlines :: Text -> Text
replaceNewlines = id
#endif

applyTransform :: Options -> Node -> IO (Either Text Node)
#ifdef ENABLE_TRANSFORMATION
applyTransform opts topNode = do
  cwd <- getCurrentDirectory
  tfConfig <- loadTransformationConfig $ cwd </> ".jbeam-edit.yaml"
  case transform (optUpdateNames opts) tfConfig topNode of
    Right (badVertexNodes, badBeamNodes, topNode') -> do
      reportInvalidNodes "Invalid vertex nodes encountered:" badVertexNodes
      reportInvalidNodes "Invalid beam nodes encountered:" badBeamNodes
      pure (Right topNode')
    Left err -> pure (Left err)
#else
applyTransform _ = pure . Right
#endif
