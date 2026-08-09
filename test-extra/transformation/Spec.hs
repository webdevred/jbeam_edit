module Spec (
  main,
) where

import Data.List (isPrefixOf, isSuffixOf)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.IsList (fromList)
import JbeamEdit.Core.Node (Node (..), NumberValue (..), expectArray)
import JbeamEdit.Core.NodePath qualified as NP
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

nodesQuery :: NP.NodePath
nodesQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "nodes"]

{- | (name, Y position) for every vertex in a top node's "nodes" section,
in file order.
-}
vertexPositionsInOrder :: Node -> [(Text, Double)]
vertexPositionsInOrder topNode =
  case NP.queryNodes nodesQuery topNode >>= NP.expectArray nodesQuery of
    Left _ -> []
    Right rows ->
      [ (name, realToFrac (nvValue yNum))
      | row <- V.toList rows
      , Just inner <- [expectArray row]
      , Just (String name) <- [inner V.!? 0]
      , name /= "id"
      , Just (Number yNum) <- [inner V.!? 2]
      ]

{- | Three small hubs (nl0, nl10, nl20; front/mid/rear), each beamed to
three of its own ordinary leaf nodes (see issue #215). At
support-threshold 20 with 12 nodes in the group, thrCount =
round(0.2*12) = 2: each hub (3 connections) clears it and becomes a
support vertex, each leaf (1 connection) doesn't. This mirrors the
support-hub shape seen in real body files (three support nodes sharing
one prefix group) at a size small enough to reason about by hand: after
the first transform, the mid/rear hubs get a trailing index (e.g. nlsl1,
nlsl2) while the front one doesn't (nlsl), and that index is exactly
what trips up the second pass. Y positions are spaced well outside the
(default 0.05) y-sorting-threshold so this test stays isolated from the
separate y-sorting-threshold banding bug (issue #214).
-}
supportRenameIdempotencyFixture :: FilePath
supportRenameIdempotencyFixture =
  "examples/regression_jbeam/support-rename-idempotency-repro.jbeam"

supportRenameIdempotencySpec :: Spec
supportRenameIdempotencySpec =
  describe "support vertex renaming"
    . it "is a fixed point: transforming the output again renames nothing further"
    $ do
      let cfg = newTransformationConfig {supportThreshold = 20}
      topNode <- parseJbeamFile supportRenameIdempotencyFixture
      case transform M.empty cfg topNode of
        Left err -> expectationFailure ("first transform failed: " ++ T.unpack err)
        Right (_, _, _, onceNode) ->
          case transform M.empty cfg onceNode of
            Left err -> expectationFailure ("second transform failed: " ++ T.unpack err)
            Right (_, _, _, twiceNode) -> do
              let once = vertexPositionsInOrder onceNode
              once `shouldNotBe` []
              vertexPositionsInOrder twiceNode `shouldBe` once

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
  supportRenameIdempotencySpec
