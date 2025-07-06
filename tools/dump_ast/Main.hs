module Main (
  main,
) where

import Core.NodeCursor
import Data.List (isSuffixOf)
import Data.Text (Text)
import Formatting
import Parsing.DSL (parseDSL)
import Parsing.Jbeam (parseNodes)
import System.Directory (getDirectoryContents)
import System.FilePath (takeBaseName, (</>))
import Text.Pretty.Simple (defaultOutputOptionsNoColor, pStringOpt)

import Data.ByteString.Lazy qualified as BL (
  readFile,
  toStrict,
 )
import Data.Text.IO qualified as TIO (writeFile)
import Data.Text.Lazy qualified as TL (toStrict)

main :: IO ()
main =
  let examplesDir = "examples"
      jbflInputDir = examplesDir </> "jbfl"
      jbeamInputDir = examplesDir </> "jbeam"
      astDir = examplesDir </> "ast"
      jbeamAstDir = astDir </> "jbeam"
      jbflAstDir = astDir </> "jbfl"
      formattedDir = examplesDir </> "formatted_jbeam"
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

saveDump :: String -> Text -> IO ()
saveDump outFile formatted =
  putStrLn ("creating " ++ outFile)
    >> TIO.writeFile outFile formatted

saveAstDump :: Show a => String -> a -> IO ()
saveAstDump outFile contents =
  let formatted = pStringOpt defaultOutputOptionsNoColor (show contents ++ "\n")
   in saveDump outFile (TL.toStrict formatted)

dumpJbflAST :: FilePath -> String -> String -> IO ()
dumpJbflAST dir outDir filename = do
  contents <- BL.readFile (dir </> filename)
  case parseDSL (BL.toStrict contents) of
    Right rs -> dump rs
    Left _ -> error $ "error " ++ filename
  where
    dump contents =
      let outFile = outDir </> takeBaseName filename ++ ".hs"
       in saveAstDump outFile contents

dumpJbeamAST :: FilePath -> String -> String -> IO ()
dumpJbeamAST dir outDir filename = do
  contents <- BL.readFile (dir </> filename)
  case parseNodes (BL.toStrict contents) of
    Right rs -> dump rs
    Left _ -> error $ "error " ++ filename
  where
    dump contents =
      let outFile = outDir </> takeBaseName filename ++ ".hs"
       in saveAstDump outFile contents

dumpFormattedJbeam :: String -> (FilePath, FilePath) -> IO ()
dumpFormattedJbeam outDir (jbeamFile, ruleFile) = do
  jbeam <- read <$> readFile jbeamFile
  rs <- read <$> readFile ruleFile
  let outFilename = takeBaseName jbeamFile ++ "jbeam_" ++ takeBaseName ruleFile ++ "jbfl.jbeam"
   in dump outFilename (formatNode rs newCursor jbeam)
  where
    dump filename contents =
      let outFile = outDir </> filename
       in saveDump outFile contents
