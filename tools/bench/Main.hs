module Main (main) where

import Criterion.Main
import Data.List (isSuffixOf)
import Data.Map qualified as M
import Data.Text qualified as T
import JbeamEdit.Formatting
import JbeamEdit.Transformation
import JbeamEdit.Transformation.Config
import System.Directory
import System.FilePath

loadAst :: Read a => FilePath -> IO a
loadAst path = read <$> readFile path

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let examplesDir = cwd </> "examples"
      astDir = examplesDir </> "ast"
      jbeamAstDir = astDir </> "jbeam"
      jbflAstDir = astDir </> "jbfl"

  let cfg = newTransformationConfig

  jbeamFiles <- filter (isSuffixOf ".hs") <$> listDirectory jbeamAstDir
  ruleFiles <- filter (isSuffixOf ".hs") <$> listDirectory jbflAstDir

  jbeamAsts <- mapM (\f -> loadAst (jbeamAstDir </> f)) jbeamFiles
  ruleAsts <- mapM (\f -> loadAst (jbflAstDir </> f)) ruleFiles

  let combos =
        [ (jFile, rFile, jAst, rAst)
        | (jFile, jAst) <- zip jbeamFiles jbeamAsts
        , (rFile, rAst) <- zip ruleFiles ruleAsts
        ]

  let benchFormat (jName, rName, jAst, rAst) =
        bench (jName ++ " + " ++ rName ++ " [format]") $
          nf (T.length . formatNode rAst) jAst

  let benchTransformFormat (jName, rName, jAst, rAst) =
        bench (jName ++ " + " ++ rName ++ " [transform+format]") $
          nf
            ( \ast ->
                case transform M.empty cfg ast of
                  Right (_, _, outAst) -> T.length (formatNode rAst outAst)
                  Left err -> error (T.unpack err)
            )
            jAst

  defaultMain
    [ bgroup "format-only" (map benchFormat combos)
    , bgroup "transform+format" (map benchTransformFormat combos)
    ]
