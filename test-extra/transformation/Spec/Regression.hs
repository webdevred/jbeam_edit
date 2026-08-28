{- | One spec per reproduced defect, each with its own fixture under
`examples/regression_jbeam/`. Unlike the fixture-driven specs in "Spec",
these assert one property rather than comparing whole output, so they say
what broke rather than that something did.
-}
module Spec.Regression (
  letterEndingNodesSpec,
  supportRenameIdempotencySpec,
  ySortingBandingSpec,
  metadataAcrossTreesSpec,
  metadataPreservedSpec,
  xColumnSortingSpec,
  noBeamsSpec,
) where

import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import GHC.IsList (toList)
import JbeamEdit.Core.Node (Node)
import JbeamEdit.Transformation
import JbeamEdit.Transformation.BeamExtraction (vertexConns)
import JbeamEdit.Transformation.Config
import JbeamEdit.Transformation.Types (
  AnnotatedVertex (..),
  VertexTree (..),
 )
import JbeamEdit.Transformation.VertexExtraction (
  determineGroup',
  getVertexForest,
  verticesQuery,
 )
import Spec.Helpers
import Test.Hspec

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
      let cfg = newTransformationConfig {ySortingThreshold = 0.1}
      topNode <- parseJbeamFile ySortingReproFixture
      case transform M.empty cfg topNode of
        Left err -> expectationFailure ("transform failed: " ++ T.unpack err)
        Right (_, _, _, resultNode) -> outOfOrderPairs 0.1 resultNode `shouldBe` []

{- | A metadata row ahead of the first vertex applies to the whole section,
and the transform writes it back out at the top, so it says nothing about
any individual vertex and must not decide where one is placed. It did
(issue #221): `newVertexTree` seeded each tree from its own leading block,
and `breakVertices` splits on prefix, so alternating nl/nr names left only
the first vertex carrying the row. `compareAV` sorts on `aMeta` ahead of
the Y band and an empty map sorts first, which dropped that one vertex at
the end of its group while its mirror on the other side stayed in front.
-}
metadataAcrossTreesFixture :: FilePath
metadataAcrossTreesFixture =
  "examples/regression_jbeam/metadata-across-trees-repro.jbeam"

metadataAcrossTreesSpec :: Spec
metadataAcrossTreesSpec =
  describe "metadata ahead of the first vertex"
    . it "does not decide where a vertex is placed"
    $ do
      topNode <- parseJbeamFile metadataAcrossTreesFixture
      case transform M.empty newTransformationConfig topNode of
        Left err -> expectationFailure ("transform failed: " ++ T.unpack err)
        Right (_, _, _, resultNode) -> do
          vertexPositionsInOrder resultNode `shouldNotBe` []
          outOfOrderPairs 0.05 resultNode `shouldBe` []

{- | The transform renames nodes, reorders them and rewrites the metadata rows,
but what any one node ends up carrying has to come out the same. The fixture
covers every shape that decides that: a key set twice in one leading block, a
key set again further down, an object on the node row overriding the section
value for that node alone, and a key set once and never again.

Today the leading block is folded with `foldr` over a left-biased `M.union`,
which puts the first row in the outermost position and lets it beat the later
one. None of the files in `examples/jbeam/` repeats a key inside one leading
block, so the whole-output fixture specs cannot see it; 485 of the 4943 stock
vehicle files do.
-}
metadataPreservedFixture :: FilePath
metadataPreservedFixture =
  "examples/regression_jbeam/last-metadata-row-repro.jbeam"

metadataPreservedSpec :: Spec
metadataPreservedSpec =
  describe "the metadata a node carries" $ do
    it "reads back from the fixture as jbeam defines it" $ do
      topNode <- parseJbeamFile metadataPreservedFixture
      let metaBefore = effectiveMetaByCoordinate topNode
      -- Guards the helper and the fixture against each other, so a failure
      -- below is the transform rather than a fixture nobody re-read.
      M.size metaBefore `shouldBe` 6
      metaNumber "nodeWeight" <$> M.lookup (0.9, -1.0, 0.1) metaBefore
        `shouldBe` Just (Just 2.0)
      metaNumber "nodeWeight" <$> M.lookup (0.9, 1.0, 0.1) metaBefore
        `shouldBe` Just (Just 3.0)
      metaNumber "nodeWeight" <$> M.lookup (0.9, 2.0, 0.1) metaBefore
        `shouldBe` Just (Just 4.0)
      metaNumber "frictionCoef" <$> M.lookup (-0.9, 0.0, 0.1) metaBefore
        `shouldBe` Just (Just 0.5)

    it "survives a transform unchanged" $ do
      topNode <- parseJbeamFile metadataPreservedFixture
      let metaBefore = effectiveMetaByCoordinate topNode
      case transform M.empty newTransformationConfig topNode of
        Left err -> expectationFailure ("transform failed: " ++ T.unpack err)
        Right (_, _, _, resultNode) -> do
          let metaAfter = effectiveMetaByCoordinate resultNode
              -- Report only the nodes whose metadata moved, with both values.
              -- Comparing the maps whole prints each of them in full, which
              -- buries which node actually broke.
              changed =
                [ (pos, expected, actual)
                | (pos, expected) <- M.toList metaBefore
                , Just actual <- [M.lookup pos metaAfter]
                , actual /= expected
                ]
          M.keys metaAfter `shouldBe` M.keys metaBefore
          changed `shouldBe` []

{- | The same gen4-style left-side positions as the y-sorting fixture, read
for a different defect. Its five frontmost nodes sit in two vertical
columns: an inner one at X 0.780/0.920/0.953 and an outer one at X
0.998/1.036, the nose face and the fender beside it. Y and Z interleave
between the two columns, so no y-sorting-threshold can separate them; only
X can. Sorting a band by Z alone therefore climbs one column, jumps to the
other and comes back, which is what the jbeam maintainer marked up on his
render.

The Y threshold here is 0.31 rather than the default because that is what puts
all five in one band, which is where the defect lives. It is not free choice:
between 0.153 and 0.16 the Y bands land on exactly the two columns, and this
assertion passes with nothing fixed at all.

`xSortingThreshold` has to be set explicitly because it has no default. There is
no number that means off (0 gives every vertex its own band, which is the most
X sorting rather than none), so the field is optional and absent means the pass
does not run at all.
-}
xColumnSortingSpec :: Spec
xColumnSortingSpec =
  describe "vertices in one Y band but different X columns" $ do
    it "keeps each column contiguous instead of interleaving them" $ do
      let inner = [(0.953, -1.967, 0.122), (0.92, -1.953, 0.439), (0.78, -1.815, 0.719)]
          outer = [(1.036, -1.807, 0.125), (0.998, -1.791, 0.473)]
      topNode <- parseJbeamFile ySortingReproFixture
      withColumnSorting topNode $ \resultNode ->
        take 5 (leftGroup (vertexCoordinatesInOrder resultNode))
          `shouldBe` inner ++ outer

    it "is a fixed point: a second transform moves nothing further" $ do
      topNode <- parseJbeamFile ySortingReproFixture
      withColumnSorting topNode $ \resultNode ->
        withColumnSorting resultNode $ \againNode ->
          vertexCoordinatesInOrder againNode
            `shouldBe` vertexCoordinatesInOrder resultNode
  where
    leftGroup = filter (\(x, _, _) -> x >= 0.09)
    columnSortingConfig =
      newTransformationConfig
        { ySortingThreshold = 0.31
        , xSortingThreshold = Just 0.2
        }
    withColumnSorting node assert =
      case transform M.empty columnSortingConfig node of
        Left err -> expectationFailure ("transform failed: " ++ T.unpack err)
        Right (_, _, _, resultNode) -> assert resultNode

{- | A jbeam file is not obliged to have a beams section. Classifying, sorting
and renaming need none: only support classification reads beams, and its
answer without them is that there are no support nodes. Issue #229.

`transform` instead fails the whole file, and the tool still exits 0, so a
run over a directory leaves such files untouched without saying why.
-}
noBeamsFixture :: FilePath
noBeamsFixture = "examples/regression_jbeam/no-beams-repro.jbeam"

noBeamsSpec :: Spec
noBeamsSpec =
  describe "a file with no beams section" $ do
    it "is transformed, keeping every node" $ do
      topNode <- parseJbeamFile noBeamsFixture
      case transform M.empty newTransformationConfig topNode of
        Left err -> expectationFailure ("transform failed: " ++ T.unpack err)
        Right (_, _, _, resultNode) ->
          length (vertexCoordinates resultNode) `shouldBe` 4

    it "counts a connection for every beamed vertex when there are beams" $ do
      topNode <- parseJbeamFile supportRenameIdempotencyFixture
      connectionCounts topNode
        `shouldBe` Right [("nl0", 3), ("nl10", 3), ("nl20", 3)]

    it "counts nothing at all when there are none" $ do
      topNode <- parseJbeamFile noBeamsFixture
      connectionCounts topNode `shouldBe` Right []

{- | The connection count `vertexConns` produces, as a sorted list so a spec
can read it. Grouping the vertices by tree type is what `transform` does
before it asks, and is repeated here because the wrapper it uses is internal.
-}
connectionCounts :: Node -> Either Text [(Text, Int)]
connectionCounts topNode = do
  (_, _, forest) <- getVertexForest brks verticesQuery topNode
  let annotated =
        concatMap (concatMap (NE.toList . tAnnotatedVertices . snd) . toList) forest
  grouped <- M.fromListWith (++) <$> mapM withGroup annotated
  (_, conns) <-
    vertexConns (maxSupportCoordinates newTransformationConfig) topNode grouped
  pure (sort [(name, count) | (name, (_, count)) <- M.toList conns])
  where
    brks = xGroupBreakpoints newTransformationConfig
    withGroup av = (,[av]) <$> determineGroup' brks (aVertex av)
