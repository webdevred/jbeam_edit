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
  lastMetadataRowSpec,
) where

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import JbeamEdit.Transformation
import JbeamEdit.Transformation.Config
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

{- | A key set twice in one leading metadata block must end up with the second
value. `topMeta` folds that block with `foldr` over a left-biased `M.union`,
which puts the first row in the outermost position and lets it win instead.
None of the files in `examples/jbeam/` repeats a key inside one leading block,
so the whole-output fixture specs cannot see this; 485 of the 4943 stock
vehicle files do.
-}
lastMetadataRowFixture :: FilePath
lastMetadataRowFixture =
  "examples/regression_jbeam/last-metadata-row-repro.jbeam"

lastMetadataRowSpec :: Spec
lastMetadataRowSpec =
  describe "a key set twice in one leading metadata block"
    . it "leaves the last row in force"
    $ do
      topNode <- parseJbeamFile lastMetadataRowFixture
      case transform M.empty newTransformationConfig topNode of
        Left err -> expectationFailure ("transform failed: " ++ T.unpack err)
        Right (_, _, _, resultNode) -> do
          let meta = effectiveMetaAtFirstVertex resultNode
          meta `shouldNotBe` M.empty
          metaNumber "nodeWeight" meta `shouldBe` Just 2.0
          metaNumber "frictionCoef" meta `shouldBe` Just 0.5
