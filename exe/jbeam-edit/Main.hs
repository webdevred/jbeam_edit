module Main (
  main,
) where

import CommandLineOptions
import Control.Monad (when)
import Data.Text (Text)
import JbeamEdit.Core.Node (Node)
import JbeamEdit.Formatting (RuleSet, formatNodeAndWrite)
import JbeamEdit.Formatting.Config
import JbeamEdit.IOUtils
import JbeamEdit.Parsing.Jbeam (parseNodes)
import System.Directory.OsPath
import System.Environment (getArgs)
import System.OsPath

#ifdef ENABLE_WINDOWS_NEWLINES
import Data.Text qualified as T
#endif

#ifdef ENABLE_TRANSFORMATION
import JbeamEdit.Transformation
import JbeamEdit.Transformation.Config
import JbeamEdit.Transformation.BeamValidation  qualified as BV
#endif

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOptions args
  case opts of
    Options {optValidateBeams = True} -> validateBeams (optInputFile opts)
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
    Right transformedNode' -> formatNodeAndWrite formattingConfig outFile transformedNode'
    Left err -> putErrorLine err

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
      mapM_ (updateOtherFiles rs updatedNames) $ map (dir </>) (filterJbeamFiles [filename] jbeamFiles)
      reportInvalidNodes "Invalid vertex nodes encountered:" badVertexNodes
      reportInvalidNodes "Invalid beam nodes encountered:" badBeamNodes
      pure (Right topNode')
    Left err -> pure (Left err)
applyTransform _ _ topNode = pure (Right topNode)
#else
applyTransform _ _  = pure . Right 
#endif

validateBeams :: Maybe OsPath -> IO ()
#ifdef ENABLE_TRANSFORMATION 
validateBeams = BV.validateBeams
#else
validateBeams = pure
#endif                
