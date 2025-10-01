module Main (
  main,
) where

import Config
import Data.List (isSuffixOf)
import Formatting
import Parsing.DSL (parseDSL)
import Parsing.Jbeam (parseNodes)
import Relude.Unsafe (read)
import System.Directory (getDirectoryContents)
import System.FilePath (dropExtension, takeBaseName, (</>))
import Text.Pretty.Simple (defaultOutputOptionsNoColor, pStringOpt)
import Transformation

import Data.Map qualified as M
import System.IO qualified as IO (readFile)

main :: IO ()
main = do
  exampleCfg <- loadTransformationConfig "examples/jbeam-edit.yaml"
  let examplesDir = "examples"
      jbflInputDir = examplesDir </> "jbfl"
      jbeamInputDir = examplesDir </> "jbeam"
      astDir = examplesDir </> "ast"
      jbeamAstDir = astDir </> "jbeam"
      jbflAstDir = astDir </> "jbfl"
      formattedDir = examplesDir </> "formatted_jbeam"
      transformedDir = examplesDir </> "transformed_jbeam"
   in do
        jbeamFiles <-
          filter (isSuffixOf ".jbeam") <$> getDirectoryContents' jbeamInputDir
        jbflFiles <- filter (isSuffixOf ".jbfl") <$> getDirectoryContents' jbflInputDir
        jbeamASTs <- mapM (dumpJbeamAST jbeamInputDir jbeamAstDir) jbeamFiles
        jbflASTs <- mapM (dumpJbflAST jbflInputDir jbflAstDir) jbflFiles
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
              jbflAstDir
              jbeamAstDir
              transformedDir
          )
          jbeamFiles
        mapM_
          ( dumpTransformedJbeam
              "cfg-example"
              exampleCfg
              jbflAstDir
              jbeamAstDir
              transformedDir
          )
          jbeamFiles

getDirectoryContents' :: FilePath -> IO [String]
getDirectoryContents' path = filter (not . isPrefixOf ".#") <$> getDirectoryContents path

saveDump :: String -> Text -> IO ()
saveDump outFile formatted =
  putStrLn ("creating " ++ outFile)
    >> writeFileText outFile formatted

saveAstDump :: Show a => String -> a -> IO ()
saveAstDump outFile contents =
  let formatted = pStringOpt defaultOutputOptionsNoColor (show contents ++ "\n")
   in saveDump outFile (toText formatted)

dumpJbflAST :: FilePath -> String -> String -> IO FilePath
dumpJbflAST dir outDir filename = do
  contents <- readFileLBS (dir </> filename)
  case parseDSL (toStrict contents) of
    Right rs -> dump rs >> pure (outDir </> filename)
    Left _ -> error $ "error " <> toText filename
  where
    dump contents =
      let outFile = outDir </> takeBaseName filename ++ ".hs"
       in saveAstDump outFile contents

dumpJbeamAST :: FilePath -> String -> String -> IO FilePath
dumpJbeamAST dir outDir filename = do
  contents <- readFileLBS (dir </> filename)
  case parseNodes (toStrict contents) of
    Right ns -> dump ns >> pure (outDir </> filename)
    Left _ -> error $ "error " <> toText filename
  where
    dump contents =
      let outFile = outDir </> takeBaseName filename ++ ".hs"
       in saveAstDump outFile contents

dumpFormattedJbeam :: FilePath -> (FilePath, FilePath) -> IO ()
dumpFormattedJbeam outDir (jbeamFile, ruleFile) = do
  jbeam <- read <$> IO.readFile (dropExtension jbeamFile ++ ".hs")
  rs <- read <$> IO.readFile (dropExtension ruleFile ++ ".hs")
  let outFilename = takeBaseName jbeamFile ++ "-" ++ takeBaseName ruleFile ++ "-jbfl.jbeam"
   in dump outFilename (formatNode rs jbeam)
  where
    dump filename contents =
      let outFile = outDir </> filename
       in saveDump outFile contents

dumpTransformedJbeam
  :: String
  -> TransformationConfig
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> IO ()
dumpTransformedJbeam cfName tfConfig rsDirPath jbeamInputAstDir outDir jbeamFile = do
  jbeam <-
    read <$> IO.readFile (jbeamInputAstDir </> (takeBaseName jbeamFile <> ".hs"))
  rs <- read <$> IO.readFile (rsDirPath </> "minimal.hs")
  let outFilename = takeBaseName jbeamFile ++ "-" ++ cfName ++ ".jbeam"
  transformedJbeam <-
    case transform M.empty tfConfig jbeam of
      Left err -> do
        putTextLn $ "error occurred during transformation" <> err
        exitFailure
      Right jbeam' -> pure jbeam'
  dump outFilename (formatNode rs transformedJbeam)
  where
    dump filename contents =
      let outFile = outDir </> filename
       in saveDump outFile contents
