{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (
  main,
) where

import Data.List (isSuffixOf)
import Formatting
import Parsing.DSL (parseDSL)
import Parsing.Jbeam (parseNodes)
import Relude.Unsafe (read)
import System.Directory (getDirectoryContents)
import System.FilePath (takeBaseName, (</>))
import Text.Pretty.Simple (defaultOutputOptionsNoColor, pStringOpt)
import Transformation

import System.IO qualified as IO (readFile)

main :: IO ()
main =
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
          filter (isSuffixOf ".jbeam") <$> getDirectoryContents jbeamInputDir
        jbflFiles <- filter (isSuffixOf ".jbfl") <$> getDirectoryContents jbflInputDir
        mapM_ (dumpJbeamAST jbeamInputDir jbeamAstDir) jbeamFiles
        mapM_ (dumpJbflAST jbflInputDir jbflAstDir) jbflFiles
        jbeamASTs <-
          filter (isSuffixOf ".hs") <$> getDirectoryContents jbeamAstDir
        jbflASTs <- filter (isSuffixOf ".hs") <$> getDirectoryContents jbflAstDir
        mapM_
          (dumpFormattedJbeam formattedDir)
          [ (jbeamAstDir </> jbeamAST, jbflAstDir </> jbflAST)
          | jbeamAST <- jbeamASTs
          , jbflAST <- jbflASTs
          ]
        mapM_ (dumpTransformedJbeam jbeamAstDir transformedDir) jbeamFiles

saveDump :: String -> Text -> IO ()
saveDump outFile formatted =
  putStrLn ("creating " ++ outFile)
    >> writeFileText outFile formatted

saveAstDump :: Show a => String -> a -> IO ()
saveAstDump outFile contents =
  let formatted = pStringOpt defaultOutputOptionsNoColor (show contents ++ "\n")
   in saveDump outFile (toText formatted)

dumpJbflAST :: FilePath -> String -> String -> IO ()
dumpJbflAST dir outDir filename = do
  contents <- readFileLBS (dir </> filename)
  case parseDSL (toStrict contents) of
    Right rs -> dump rs
    Left _ -> error $ "error " <> toText filename
  where
    dump contents =
      let outFile = outDir </> takeBaseName filename ++ ".hs"
       in saveAstDump outFile contents

dumpJbeamAST :: FilePath -> String -> String -> IO ()
dumpJbeamAST dir outDir filename = do
  contents <- readFileLBS (dir </> filename)
  case parseNodes (toStrict contents) of
    Right rs -> dump rs
    Left _ -> error $ "error " <> toText filename
  where
    dump contents =
      let outFile = outDir </> takeBaseName filename ++ ".hs"
       in saveAstDump outFile contents

dumpFormattedJbeam :: FilePath -> (FilePath, FilePath) -> IO ()
dumpFormattedJbeam outDir (jbeamFile, ruleFile) = do
  jbeam <- read <$> IO.readFile jbeamFile
  rs <- read <$> IO.readFile ruleFile
  let outFilename = takeBaseName jbeamFile ++ "-" ++ takeBaseName ruleFile ++ "-jbfl.jbeam"
   in dump outFilename (formatNode rs jbeam)
  where
    dump filename contents =
      let outFile = outDir </> filename
       in saveDump outFile contents

dumpTransformedJbeam :: FilePath -> FilePath -> FilePath -> IO ()
dumpTransformedJbeam jbeamInputAstDir outDir jbeamFile = do
  jbeam <-
    read <$> IO.readFile (jbeamInputAstDir </> (takeBaseName jbeamFile <> ".hs"))
  let outFilename = takeBaseName jbeamFile ++ ".jbeam"
  transformedJbeam <-
    case transform jbeam of
      Left err -> do
        putTextLn $ "error occurred during transformation" <> err
        exitFailure
      Right jbeam' -> pure jbeam'
  dump outFilename (formatNode newRuleSet transformedJbeam)
  where
    dump filename contents =
      let outFile = outDir </> filename
       in saveDump outFile contents
