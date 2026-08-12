module Spec (
  main,
) where

import Data.Char (isDigit)
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
  mapM_ (testInputFile "cfg-default" newTransformationConfig) inputFiles
  mapM_ (testInputFile "cfg-example" tfConfig) inputFiles
  beamValidationSpec
  ySortingBandingSpec
