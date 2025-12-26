{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Criterion.Main
import Data.ByteString.Lazy qualified as BS
import Data.List (isSuffixOf)
import Data.Map qualified as M
import Data.Text qualified as T
import JbeamEdit.Formatting
import JbeamEdit.Parsing.DSL (parseDSL)
import JbeamEdit.Parsing.Jbeam (parseNodes)
import JbeamEdit.Transformation
import JbeamEdit.Transformation.Config
import System.Directory
import System.FilePath

loadFile :: FilePath -> IO BS.ByteString
loadFile = BS.readFile

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let jbeamDir = cwd </> "examples" </> "jbeam"
      jbflDir = cwd </> "examples" </> "jbfl"

  jbeamFiles <- filter (isSuffixOf ".jbeam") <$> listDirectory jbeamDir
  jbflFiles <- filter (isSuffixOf ".jbfl") <$> listDirectory jbflDir

  let cfg = newTransformationConfig

  let combos =
        [ (jFile, rFile)
        | jFile <- jbeamFiles
        , rFile <- jbflFiles
        ]

  let benchFormat (jName, rName) =
        bench (jName ++ " + " ++ rName ++ " [format]") . nfIO $
          ( do
              jText <- loadFile (jbeamDir </> jName)
              rText <- loadFile (jbflDir </> rName)
              let (Right jAst) = parseNodes jText
                  (Right rAst) = parseDSL rText
              pure $ formatNode rAst jAst
          )

  let benchTransformFormat (jName, rName) =
        bench (jName ++ " + " ++ rName ++ " [transform+format]") . nfIO $
          ( do
              jText <- loadFile (jbeamDir </> jName)
              rText <- loadFile (jbflDir </> rName)
              let (Right jAst) = parseNodes jText
                  (Right rAst) = parseDSL rText
              case transform M.empty cfg jAst of
                Right (_, _, _, outAst) -> pure $ formatNode rAst outAst
                Left err -> error (T.unpack err)
          )

  defaultMain
    [ bgroup "format-only" (map benchFormat combos)
    , bgroup "transform+format" (map benchTransformFormat combos)
    ]
