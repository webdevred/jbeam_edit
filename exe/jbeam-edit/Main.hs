module Main (
  main,
) where

import CommandLineOptions
import Control.Monad (when)
import Data.ByteString.Lazy qualified as LBS (fromStrict)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import JbeamEdit.Core.Node (Node)
import JbeamEdit.Core.NodeCursor
import JbeamEdit.Formatting (RuleSet, formatNode)
import JbeamEdit.Formatting.Config
import JbeamEdit.IOUtils
import JbeamEdit.Parsing.Jbeam (parseNodes)
import System.Directory.OsPath
import System.Environment (getArgs)
import System.File.OsPath qualified as OS (writeFile)
import System.OsPath

#ifdef ENABLE_WINDOWS_NEWLINES
import Data.Text qualified as T
#endif

#ifdef ENABLE_TRANSFORMATION
import JbeamEdit.Transformation
import JbeamEdit.Transformation.Types    
import JbeamEdit.Transformation.Config
#endif

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOptions args
  case opts of
    Options {optCopyJbflConfig = Just configType} -> copyToConfigDir configType
    _ -> editFile opts

createBackupFile :: OsPath -> Options -> IO ()
createBackupFile filename opts = do
  let backupFilename = dropExtension filename <.> unsafeEncodeUtf ".bak.jbeam"
  doesExist <- doesFileExist filename
  when
    (not (optInPlace opts) && doesExist)
    (copyFile filename backupFilename)

editFile :: Options -> IO ()
editFile opts = do
  formattingConfig <- readFormattingConfig Nothing
  case optInputFile opts of
    Just filename -> do
      createBackupFile filename opts
      contents <- tryReadFile [] filename
      case contents >>= parseNodes of
        Right ns -> processNodes opts filename ns formattingConfig
        Left err -> putErrorLine err
    Nothing -> putErrorLine "missing arg filename"

processNodes :: Options -> OsPath -> Node -> RuleSet -> IO ()
processNodes opts outFile nodes formattingConfig = do
  transformedNode <- applyTransform formattingConfig opts nodes
  case transformedNode of
    Right transformedNode' ->
      OS.writeFile outFile
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

applyTransform :: RuleSet -> Options -> Node -> IO (Either Text Node)
#ifdef ENABLE_TRANSFORMATION
applyTransform rs opts@(Options {optTransformation = True, optInputFile = Just inputFile}) topNode = do
  cwd <- getCurrentDirectory
  tfConfig <- loadTransformationConfig $ cwd </> transformationConfigFile
  let dir = takeDirectory inputFile
      filename = takeFileName inputFile
  jbeamFiles <- listDirectory dir
  case transform (optUpdateNames opts) tfConfig topNode of
    Right (badVertexNodes, badBeamNodes, updatedNames, topNode') -> do
      print updatedNames
      mapM_ (updateOtherFiles rs updatedNames) (map (dir </>) . filter ((/=) filename) $ jbeamFiles)
      reportInvalidNodes "Invalid vertex nodes encountered:" badVertexNodes
      reportInvalidNodes "Invalid beam nodes encountered:" badBeamNodes
      pure (Right topNode')
    Left err -> pure (Left err)
applyTransform _ _ topNode = pure (Right topNode)

updateOtherFiles :: RuleSet -> UpdateNamesMap -> OsPath ->  IO ()
updateOtherFiles formattingConfig updatedNames filepath  = do
  contents <- tryReadFile [] filepath
  case contents >>= parseNodes of
    Right transformedNode' ->
      OS.writeFile filepath
        . LBS.fromStrict
        . encodeUtf8
        . replaceNewlines
        . formatNode formattingConfig
        . findAndUpdateTextInNode updatedNames newCursor
        $ transformedNode'
    Left err -> putErrorLine err                           
#else
applyTransform _ _  = pure . Right 
#endif
