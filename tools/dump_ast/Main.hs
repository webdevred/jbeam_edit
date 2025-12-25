module Main (
  main,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import JbeamEdit.Formatting
import JbeamEdit.Parsing.DSL (parseDSL)
import JbeamEdit.Parsing.Jbeam (parseNodes)
import JbeamEdit.Transformation
import JbeamEdit.Transformation.Config
import JbeamEdit.Transformation.Types
import System.Directory (copyFile, getDirectoryContents)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, takeBaseName, (</>))
import System.IO qualified as IO (readFile)
import System.OsPath qualified as OS (unsafeEncodeUtf, (</>))
import Text.Pretty.Simple (
  StringOutputStyle (..),
  defaultOutputOptionsNoColor,
  outputOptionsStringStyle,
  pStringOpt,
 )

main :: IO ()
main = do
  exampleCfg <-
    loadTransformationConfig $
      OS.unsafeEncodeUtf "examples" OS.</> OS.unsafeEncodeUtf "jbeam-edit.yaml"
  let examplesDir = "examples"
      jbflInputDir = examplesDir </> "jbfl"
      jbeamInputDir = examplesDir </> "jbeam"
      astDir = examplesDir </> "ast"
      jbeamAstDir = astDir </> "jbeam"
      jbflAstDir = astDir </> "jbfl"
      lspDir = "test-extra" </> "language-server" </> "data"
      formattedDir = examplesDir </> "formatted_jbeam"
      transformedDir = examplesDir </> "transformed_jbeam"
   in do
        jbeamFiles <-
          filter (isSuffixOf ".jbeam") <$> getDirectoryContents' jbeamInputDir
        jbflFiles <- filter (isSuffixOf ".jbfl") <$> getDirectoryContents' jbflInputDir
        jbeamASTs <- mapM (dumpJbeamAST jbeamInputDir jbeamAstDir) jbeamFiles
        jbflASTs <- mapM (dumpJbflAST jbflInputDir jbflAstDir) jbflFiles
        dumpFormattedJbeam'
          jbflInputDir
          lspDir
          (lspDir </> "custom-minimal.jbfl")
          (jbeamAstDir </> "fender.jbeam")
        mapM_
          (dumpFormattedJbeam formattedDir)
          [ (jbeamAST, jbflAST)
          | jbeamAST <- jbeamASTs
          , jbflAST <- jbflASTs
          ]
        mapM_
          ( dumpTransformedJbeam
              "cfg-default"
              newTransformationConfig
              jbeamInputDir
              jbflAstDir
              jbeamAstDir
              transformedDir
          )
          jbeamFiles
        mapM_
          ( dumpTransformedJbeam
              "cfg-example"
              exampleCfg
              jbeamInputDir
              jbflAstDir
              jbeamAstDir
              transformedDir
          )
          jbeamFiles

dumpFormattedJbeam' :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
dumpFormattedJbeam' jbflDir outDir ruleFile jbeamFile = do
  jbeam <- read <$> IO.readFile (dropExtension jbeamFile ++ ".hs")
  (Right rs) <- parseDSL <$> LBS.readFile ruleFile
  (Right rs') <- parseDSL <$> LBS.readFile (jbflDir </> "minimal.jbfl")
  let outFilename = takeBaseName jbeamFile ++ "-" ++ takeBaseName ruleFile ++ "-jbfl.jbeam"
   in dump outFilename (T.unpack $ formatNode (rs <> rs') jbeam)
  where
    dump filename contents =
      let outFile = outDir </> filename
       in saveDump outFile contents

getDirectoryContents' :: FilePath -> IO [String]
getDirectoryContents' path = filter (not . isPrefixOf ".#") <$> getDirectoryContents path

saveDump :: String -> String -> IO ()
saveDump outFile formatted =
  putStrLn ("creating " ++ outFile)
    >> writeFile outFile formatted

saveAstDump :: Show a => String -> a -> IO ()
saveAstDump outFile contents =
  let formatted =
        pStringOpt
          defaultOutputOptionsNoColor {outputOptionsStringStyle = Literal}
          (show contents ++ "\n")
   in saveDump outFile (LT.unpack formatted)

dumpJbflAST :: FilePath -> String -> String -> IO FilePath
dumpJbflAST dir outDir filename = do
  contents <- LBS.readFile (dir </> filename)
  case parseDSL contents of
    Right rs -> dump rs >> pure (outDir </> filename)
    Left _ -> error $ "error " ++ filename
  where
    dump contents =
      let outFile = outDir </> takeBaseName filename ++ ".hs"
       in saveAstDump outFile contents

dumpJbeamAST :: FilePath -> String -> String -> IO FilePath
dumpJbeamAST dir outDir filename = do
  contents <- LBS.readFile (dir </> filename)
  case parseNodes contents of
    Right ns -> dump ns >> pure (outDir </> filename)
    Left _ -> error $ "error " <> filename
  where
    dump contents =
      let outFile = outDir </> takeBaseName filename ++ ".hs"
       in saveAstDump outFile contents

dumpFormattedJbeam :: FilePath -> (FilePath, FilePath) -> IO ()
dumpFormattedJbeam outDir (jbeamFile, ruleFile) = do
  jbeam <- read <$> IO.readFile (dropExtension jbeamFile ++ ".hs")
  rs <- read <$> IO.readFile (dropExtension ruleFile ++ ".hs")
  let outFilename = takeBaseName jbeamFile ++ "-" ++ takeBaseName ruleFile ++ "-jbfl.jbeam"
   in dump outFilename (T.unpack $ formatNode rs jbeam)
  where
    dump filename contents =
      let outFile = outDir </> filename
       in saveDump outFile contents

fenderAfterFrame
  :: String
  -> RuleSet
  -> UpdateNamesMap
  -> FilePath
  -> FilePath
  -> String
  -> IO ()
fenderAfterFrame "frame" rs updateNames input out cfName = do
  let outFile = out </> "fender-after-frame-" ++ cfName ++ ".jbeam"
  copyFile input outFile
  putStrLn ("creating " ++ outFile)
  updateOtherFiles
    rs
    updateNames
    (OS.unsafeEncodeUtf outFile)
fenderAfterFrame _ _ _ _ _ _ = pure ()

dumpTransformedJbeam
  :: String
  -> TransformationConfig
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> IO ()
dumpTransformedJbeam cfName tfConfig jbeamDir rsDirPath jbeamInputAstDir outDir jbeamFile = do
  jbeam <-
    read <$> IO.readFile (jbeamInputAstDir </> (takeBaseName jbeamFile <> ".hs"))
  rs <- read <$> IO.readFile (rsDirPath </> "minimal.hs")
  let outFilename = takeBaseName jbeamFile ++ "-" ++ cfName ++ ".jbeam"
  transformedJbeam <-
    case transform M.empty tfConfig jbeam of
      Left err -> do
        putStrLn $ "error occurred during transformation" ++ T.unpack err
        exitFailure
      Right (_, _, updatedNames, jbeam') ->
        fenderAfterFrame
          (takeBaseName jbeamFile)
          rs
          updatedNames
          (jbeamDir </> "fender.jbeam")
          outDir
          cfName
          >> pure jbeam'
  dump outFilename (T.unpack $ formatNode rs transformedJbeam)
  where
    dump filename contents =
      let outFile = outDir </> filename
       in saveDump outFile contents
