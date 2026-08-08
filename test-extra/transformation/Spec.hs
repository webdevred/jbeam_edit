module Spec (
  main,
) where

import Data.List (isPrefixOf, isSuffixOf)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import JbeamEdit.Core.Node (Node)
import JbeamEdit.Formatting
import JbeamEdit.IOUtils (tryReadFile)
import JbeamEdit.Parsing.Jbeam (parseNodes)
import JbeamEdit.Transformation
import JbeamEdit.Transformation.BeamExtraction (beamInKnownSet)
import JbeamEdit.Transformation.BeamValidation
import JbeamEdit.Transformation.Config
import JbeamEdit.Transformation.Types (Beam)
import System.Directory (getDirectoryContents)
import System.OsPath
import Test.Hspec

listFilesInDir
  :: FilePath
  -> IO [String]
listFilesInDir dir =
  filter (\f -> isSuffixOf ".hs" f && not (".#" `isPrefixOf` f))
    <$> getDirectoryContents dir

topNodeSpec
  :: RuleSet -> String -> TransformationConfig -> FilePath -> FilePath -> Spec
topNodeSpec rs cfName tfConfig inFilename outFilename = do
  let inputPath = "examples/ast/jbeam/" ++ inFilename
  input <- runIO $ readFile inputPath
  output <- runIO $ readFile outFilename
  let desc =
        "with "
          ++ cfName
          ++ ": should transform AST in "
          ++ inFilename
          ++ " to Jbeam in "
          ++ outFilename
      transformAndFormat =
        do
          (_, _, _, node) <- transform M.empty tfConfig (read input)
          Right (formatNode rs node)
  describe desc . it "works" $ transformAndFormat `shouldBe` Right (T.pack output)

parseJbeamFile :: FilePath -> IO Node
parseJbeamFile path = do
  let osPath = unsafeEncodeUtf path
  contents <- tryReadFile [] osPath
  case contents >>= parseNodes of
    Right node -> pure node
    Left err -> fail $ "Failed to parse " ++ path ++ ": " ++ T.unpack err

beamValidationSpec :: Spec
beamValidationSpec = do
  frameNode <-
    runIO $ parseJbeamFile "examples/transformed_jbeam/frame-cfg-default.jbeam"
  fenderNode <-
    runIO $
      parseJbeamFile "examples/transformed_jbeam/fender-after-frame-cfg-default.jbeam"
  let allVerts =
        foldMap
          (either (error . T.unpack) id . extractVertexNames)
          [frameNode, fenderNode]
      allBeams :: [(String, [Beam])]
      allBeams =
        [ ("frame", extractFileBeams frameNode)
        , ("fender", extractFileBeams fenderNode)
        ]
      internalBeams =
        [ (name, filter (beamInKnownSet allVerts) beams)
        | (name, beams) <- allBeams
        ]
  describe "beam validation across frame and fender" $ do
    it "has no invalid beam references for internal beams" $ do
      let invalidNames = foldMap (\(_, _, inv) -> inv)
          allInvalid =
            foldMap
              (\(_, beams) -> invalidNames (findInvalidRefs allVerts beams))
              internalBeams
      allInvalid `shouldBe` S.empty

    it "has no duplicate beams" $
      findDuplicateBeams internalBeams `shouldBe` []

{- | Real jbeam files commonly interleave per-triangle metadata objects
(e.g. `{"groundModel": "metal"}`) among triangle rows. That is normal,
not malformed input. `getTriangleVertexNames` used to fail the whole
`transform` call on the first such row instead of skipping it.
-}
trianglesWithMetadataFixture :: FilePath
trianglesWithMetadataFixture = "examples/regression_jbeam/triangles-with-metadata-repro.jbeam"

triangleMetadataSpec :: Spec
triangleMetadataSpec =
  describe "triangles with inline metadata rows"
    . it "does not fail transform"
    $ do
      topNode <- parseJbeamFile trianglesWithMetadataFixture
      case transform M.empty newTransformationConfig topNode of
        Left err -> expectationFailure ("transform failed: " ++ T.unpack err)
        Right _ -> pure ()

main :: IO ()
main = hspec $ do
  let exampleConfigPath = unsafeEncodeUtf "examples/jbeam-edit.yaml"
  rs <- runIO $ readFile "examples/ast/jbfl/minimal.hs"
  tfConfig <- runIO $ loadTransformationConfig exampleConfigPath
  inputFiles <-
    runIO $ listFilesInDir "examples/ast/jbeam"
  let outputFile cfName inFile =
        "examples/transformed_jbeam/"
          ++ takeWhile (/= '.') inFile
          ++ "-"
          ++ cfName
          ++ ".jbeam"
      testInputFile cfName tfConfig' inFile = topNodeSpec (read rs) cfName tfConfig' inFile (outputFile cfName inFile)
  mapM_ (testInputFile "cfg-default" newTransformationConfig) inputFiles
  mapM_ (testInputFile "cfg-example" tfConfig) inputFiles
  beamValidationSpec
  triangleMetadataSpec
