module Spec (
  main,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
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
  describe desc . it "works" $
    if "suspension" `isInfixOf` outFilename
      then
        pendingWith "metadata is not carried across vertex tree boundaries, issue #221"
      else check

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

{- | (name, Y position) for every vertex in a transformed top node's
"nodes" section, in file order (i.e. the order `transform` actually
wrote them out in). Read straight back out of the output rather than
correlated against the input names, since `transform` renames vertices.
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

{- | Every vertex coordinate in a top node's "nodes" section. Positions
survive renaming, so they identify a vertex across a transform.
-}
vertexCoordinates :: Node -> Set (Double, Double, Double)
vertexCoordinates topNode =
  case NP.queryNodes nodesQuery topNode >>= NP.expectArray nodesQuery of
    Left _ -> S.empty
    Right rows ->
      S.fromList
        [ (realToFrac (nvValue x), realToFrac (nvValue y), realToFrac (nvValue z))
        | row <- V.toList rows
        , Just inner <- [expectArray row]
        , Just (String name) <- [inner V.!? 0]
        , name /= "id"
        , Just (Number x) <- [inner V.!? 1]
        , Just (Number y) <- [inner V.!? 2]
        , Just (Number z) <- [inner V.!? 3]
        ]

{- | Names ending in a letter rather than a digit all map to the same
SupportKey, so an insert that replaced instead of merged used to drop
every group but the last.
-}
letterEndingNodesFixture :: FilePath
letterEndingNodesFixture =
  "examples/regression_jbeam/letter-ending-nodes-repro.jbeam"

letterEndingNodesSpec :: Spec
letterEndingNodesSpec =
  describe "letter-ending node names"
    . it "keeps every vertex through a transform"
    $ do
      topNode <- parseJbeamFile letterEndingNodesFixture
      let expected = vertexCoordinates topNode
      expected `shouldNotBe` S.empty
      case transform M.empty newTransformationConfig topNode of
        Left err -> expectationFailure ("transform failed: " ++ T.unpack err)
        Right (_, _, _, resultNode) ->
          vertexCoordinates resultNode `shouldBe` expected

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

{- | Real left-side structural node positions from a NASCAR gen4-style body
file (see issue #214). This specific spacing reproduces a real transform
run: with y-sorting-threshold 0.1, the frontmost node (nl0, Y=-1.967) ends
up sorted to the back of its group instead of the front.
-}
ySortingReproFixture :: FilePath
ySortingReproFixture = "examples/regression_jbeam/y-sorting-repro.jbeam"

ySortingBandingSpec :: Spec
ySortingBandingSpec =
  describe "y-sorting-threshold"
    . it
      "never places a node behind another node that is more than the threshold further forward"
    $ do
      let thr = 0.1 :: Double
          cfg = newTransformationConfig {ySortingThreshold = 0.1}
      topNode <- parseJbeamFile ySortingReproFixture
      case transform M.empty cfg topNode of
        Left err -> expectationFailure ("transform failed: " ++ T.unpack err)
        Right (_, _, _, resultNode) -> do
          let positions = zip (vertexPositionsInOrder resultNode) [0 :: Int ..]
              groupPrefix = T.dropWhileEnd isDigit
              -- Only compare nodes within the same output group (e.g.
              -- "nll", "nlm"): a Left-tree node and a Middle-tree node
              -- are unrelated blocks in the file, not a single ordered
              -- sequence, so their relative position isn't meaningful.
              outOfOrder =
                [ (y1, y2)
                | ((n1, y1), i1) <- positions
                , ((n2, y2), i2) <- positions
                , i1 < i2
                , groupPrefix n1 == groupPrefix n2
                , y1 - y2 > thr
                ]
          outOfOrder `shouldBe` []

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
  beamValidationSpec
  supportRenameIdempotencySpec
  letterEndingNodesSpec
  ySortingBandingSpec
