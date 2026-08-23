{- | The fixture-driven half of the transformation suite: every file in
`examples/ast/jbeam/` transformed under both configs and compared against
its expected output, plus the cross-file beam checks. One reproduced defect
per spec lives in "Spec.Regression" instead.
-}
module Spec (
  main,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import JbeamEdit.Formatting
import JbeamEdit.Parsing.Jbeam (parseNodes)
import JbeamEdit.Transformation
import JbeamEdit.Transformation.BeamExtraction (beamInKnownSet)
import JbeamEdit.Transformation.BeamValidation
import JbeamEdit.Transformation.Config
import JbeamEdit.Transformation.Types (Beam)
import Spec.Config (configParsingSpec)
import Spec.Helpers (parseJbeamFile)
import Spec.Regression
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

{- | Transforming an already transformed file must produce the same text
again. Unlike 'supportRenameIdempotencySpec' this compares the whole
formatted output, so it also covers comments, metadata and beam references.
-}
fixedPointSpec
  :: RuleSet -> String -> TransformationConfig -> FilePath -> Spec
fixedPointSpec rs cfName tfConfig outFilename = do
  output <- runIO $ readFile outFilename
  let desc =
        "with "
          ++ cfName
          ++ ": transforming "
          ++ outFilename
          ++ " again should leave it unchanged"
      -- Parse the same text the result is compared against, rather than
      -- reading the file a second time, and drop the carriage returns a
      -- Windows checkout leaves behind. formatNode always emits LF, so
      -- the line endings the file happens to carry are not part of what
      -- this spec is asserting.
      expected = T.replace "\r\n" "\n" (T.pack output)
      check =
        case parseNodes (LBS.fromStrict (encodeUtf8 expected)) of
          Left err ->
            expectationFailure
              ("failed to parse " ++ outFilename ++ ": " ++ T.unpack err)
          Right node ->
            case transform M.empty tfConfig node of
              Left err -> expectationFailure ("transform failed: " ++ T.unpack err)
              Right (_, _, _, again) -> formatNode rs again `shouldBe` expected
  describe desc . it "works" $ check

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
      testFixedPoint cfName tfConfig' inFile =
        fixedPointSpec (read rs) cfName tfConfig' (outputFile cfName inFile)
  mapM_ (testInputFile "cfg-default" newTransformationConfig) inputFiles
  mapM_ (testInputFile "cfg-example" tfConfig) inputFiles
  mapM_ (testFixedPoint "cfg-default" newTransformationConfig) inputFiles
  mapM_ (testFixedPoint "cfg-example" tfConfig) inputFiles
  configParsingSpec
  beamValidationSpec
  supportRenameIdempotencySpec
  letterEndingNodesSpec
  ySortingBandingSpec
  xColumnSortingSpec
  metadataAcrossTreesSpec
  metadataPreservedSpec
  triangleMetadataSpec
