module Main (
  main,
) where

import Data.List (isSuffixOf)
import Parsing.DSL (parseDSL)
import Parsing.Jbeam (parseNodes)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import Text.Pretty.Simple (defaultOutputOptionsNoColor, pStringOpt)

import Data.ByteString.Lazy qualified as BL (
  readFile,
  toStrict,
 )
import Data.Text.Lazy.IO qualified as TLIO (writeFile)

main :: IO ()
main =
  let jbflInputDir = "examples" </> "jbfl"
      jbeamInputDir = "examples" </> "jbeam"
   in do
        jbeamFiles <-
          filter (isSuffixOf ".jbeam") <$> getDirectoryContents jbeamInputDir
        jbflFiles <- filter (isSuffixOf ".jbfl") <$> getDirectoryContents jbflInputDir
        mapM_ (dumpJbflAST jbflInputDir) jbflFiles
        mapM_ (dumpJbeamAST jbeamInputDir) jbeamFiles

saveDump :: Show a => String -> a -> IO ()
saveDump outFile contents =
  let formatted = pStringOpt defaultOutputOptionsNoColor (show contents ++ "\n")
   in putStrLn ("creating " ++ outFile)
        >> TLIO.writeFile outFile formatted

astDir :: FilePath
astDir = "examples" </> "ast"

dumpJbflAST :: String -> String -> IO ()
dumpJbflAST dir filename = do
  contents <- BL.readFile (dir </> filename)
  case parseDSL (BL.toStrict contents) of
    Right rs -> dump rs
    Left _ -> error $ "error " ++ filename
  where
    dump contents =
      let outFile = astDir </> "jbfl" </> takeWhile (/= '.') filename ++ ".hs"
       in saveDump outFile contents

dumpJbeamAST :: String -> String -> IO ()
dumpJbeamAST dir filename = do
  contents <- BL.readFile (dir </> filename)
  case parseNodes (BL.toStrict contents) of
    Right rs -> dump rs
    Left _ -> error $ "error " ++ filename
  where
    dump contents =
      let outFile = astDir </> "jbeam" </> takeWhile (/= '.') filename ++ ".hs"
       in saveDump outFile contents
