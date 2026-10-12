{- | The reading order the jbeam maintainer marked up, part by part.

Each spec states the order he asked for, so it describes the target rather than
what the tool produces today. The axes are set on the config record directly:
no YAML key reads them yet, and the name of the threshold key is still open, so
a spec that spelled either one out would be inventing config syntax.

The part fixtures are cut from the files the maintainer sent. The hood and the
bumper keep their first part and drop the second, which `transform` never reads,
and the body file holds one part and is copied whole. The last spec uses an
invented fixture instead, because the shape it covers is in none of the three.
-}
module Spec.ColumnOrder (
  columnOrderSpec,
  partFixedPointSpec,
) where

import Data.Map qualified as M
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import JbeamEdit.Transformation (transform)
import JbeamEdit.Transformation.Config (
  TransformationConfig (..),
  defaultSortAxes,
  newTransformationConfig,
 )
import JbeamEdit.Transformation.Types (Axis (..), SortAxes (..))
import Spec.Helpers (
  originalNamesInOutputOrder,
  parseJbeamFile,
  vertexNamesInOrder,
 )
import Test.Hspec

hoodFixture :: FilePath
hoodFixture = "examples/regression_jbeam/hood-columns-repro.jbeam"

frontBumperFixture :: FilePath
frontBumperFixture = "examples/regression_jbeam/front-bumper-columns-repro.jbeam"

bodyFixture :: FilePath
bodyFixture = "examples/regression_jbeam/body-columns-repro.jbeam"

middleGroupFixture :: FilePath
middleGroupFixture = "examples/regression_jbeam/middle-group-sign-repro.jbeam"

{- | A part with nodes and no beams section at all. Reading the columns off the
beams finds no chain here, so the order has to fall back on the bands and the
walk axis, which is what the spec below pins.
-}
noBeamsFixture :: FilePath
noBeamsFixture = "examples/regression_jbeam/no-beams-repro.jbeam"

{- | The order marked up for one side of the body shell: the front edge read as
two columns, the rear column read from its lowest node, and everything between
them exactly as the file already has it.
-}
bodySideOrder :: Text -> [Text]
bodySideOrder prefix =
  map named ([0, 2, 4, 1, 3] <> [5 .. 43] <> [47, 44, 45, 46])
  where
    named index = prefix <> T.pack (show (index :: Int))

{- | The order a transform wrote the vertices out in, under the names they had
in the file, which is what the marked up orders are written in.
-}
orderWith :: SortAxes -> Scientific -> FilePath -> IO [Text]
orderWith axes threshold path = do
  topNode <- parseJbeamFile path
  let config =
        newTransformationConfig
          { sortAxes = axes
          , ySortingThreshold = threshold
          }
  case transform M.empty config topNode of
    Left err -> fail ("transform failed: " ++ T.unpack err)
    Right (_, _, _, resultNode) ->
      pure (originalNamesInOutputOrder topNode resultNode)

group :: Text -> [Text] -> [Text]
group prefix = filter (T.isPrefixOf prefix)

columnOrderSpec :: Spec
columnOrderSpec = describe "the reading order each part asks for" $ do
  it "reads the hood one column at a time, the outer column first" $ do
    pendingWith
      "The hood's columns run across the car, so X groups them, and the \
      \column by the fender is wanted first. X is compared as written, so \
      \the group sorts from the centre line outwards and the inner column \
      \comes out first: hdl3 hdl4 hdl5 hdl6 hdl0 hdl1 hdl2. Comparing the \
      \distance from the centre line, negated where X groups, turns this \
      \green on its own. The right side already reads the wanted way, \
      \because its X values are negative."
    order <- orderWith (SortAxes AxisX AxisY AxisZ) 0.23 hoodFixture
    order
      `shouldBe` [ "hdl0"
                 , "hdl1"
                 , "hdl2"
                 , "hdl3"
                 , "hdl4"
                 , "hdl5"
                 , "hdl6"
                 , "hdr0"
                 , "hdr1"
                 , "hdr2"
                 , "hdr3"
                 , "hdr4"
                 , "hdr5"
                 , "hdr6"
                 , "hd_supp"
                 ]

  it "reads each column of the front bumper from the bottom up" $ do
    pendingWith
      "The bumper's columns are upright stacks, so X groups them and Z walks \
      \them, and two things are missing. X is compared as written, so the \
      \innermost column comes first and the order is fbl5 fbl6 fbl7 fbl3 \
      \fbl4 fbl1 fbl2 fbl0. Comparing the distance from the centre line, \
      \negated where X groups, gets the columns the right way round but \
      \still gives fbl3 fbl0 fbl1 fbl2 at the outer end, because at 0.23 \
      \fbl0, fbl1 and fbl3 share a band and Z reads fbl3 first as the \
      \lowest of them. The rest needs the columns taken from the beams, \
      \where fbl0, fbl1 and fbl2 form one chain and fbl3, fbl4 another."
    order <- orderWith (SortAxes AxisX AxisZ AxisY) 0.23 frontBumperFixture
    group "fbl" order
      `shouldBe` ["fbl0", "fbl1", "fbl2", "fbl3", "fbl4", "fbl5", "fbl6", "fbl7"]
    group "fbm" order `shouldBe` ["fbm0", "fbm1", "fbm2"]

  it "reads the bumper's right side as the mirror of its left" $ do
    pendingWith
      "The right side holds a beam between fbr1 and fbm0 where the left side \
      \has one between fbl1 and fbl0. Of the 31 beams here whose two nodes \
      \both have a mirror node, that is the only pair with no mirror image. \
      \Today the side reads \
      \fbr3 fbr0 fbr1 fbr2 fbr4 fbr5 fbr6 fbr7. Its X values are negative, so \
      \comparing X as written already puts the outer column first here, and \
      \the columns from the beams are the only thing missing. Those would \
      \read it fbr1 fbr2 fbr3 fbr0 fbr4 fbr5 fbr6 fbr7 while that beam \
      \stands, since it leaves fbr0 out of the chain its mirror image is in. \
      \So the mirrored order asserted here holds only once that beam has a \
      \mirror image, and the fixture is left exactly as it was sent."
    order <- orderWith (SortAxes AxisX AxisZ AxisY) 0.23 frontBumperFixture
    group "fbr" order
      `shouldBe` ["fbr0", "fbr1", "fbr2", "fbr3", "fbr4", "fbr5", "fbr6", "fbr7"]

  it "reads the body's front edge as two columns, and the rest as it stands" $ do
    pendingWith
      "The front edge leans: its first column runs 0.152 along Y as it rises \
      \and the next one starts only 0.008 behind where that column ends. A \
      \threshold of 0.16 does separate the two, and it splits stations \
      \further back that 0.23 keeps whole: the side falls into 17 bands at \
      \0.16 against 14 at 0.23. All five nodes share one band at 0.23 \
      \and Z then reads them bl0 bl1 bl2 bl3 bl4, which is the order the \
      \file already has and still not the order asked for. Taking the \
      \columns from the beams gives bl0 bl2 bl4 bl1 bl3. This spec covers the \
      \whole side, so the rear column below is part of it and needs the \
      \hold-back as well."
    order <- orderWith (SortAxes AxisY AxisZ AxisX) 0.23 bodyFixture
    group "bl" order `shouldBe` bodySideOrder "bl"

  it "reads the body's right side the same way as its left" $ do
    pendingWith
      "Today the right side reads br0 br1 br2 br3 br4 and ends br45 br46 \
      \br47 br44, so it needs the columns from the beams and the hold-back \
      \for its rear column, as the left side does. X is only the tie break \
      \with these axes, and this side comes out the same whether it is read \
      \as written or as a distance from the centre line, so nothing here \
      \rests on the two sides being compared the same way."
    order <- orderWith (SortAxes AxisY AxisZ AxisX) 0.23 bodyFixture
    group "br" order `shouldBe` bodySideOrder "br"

  it "never reads the rear column before its lowest node" $ do
    pendingWith
      "The rear column climbs 0.461 in Z and travels 0.299 towards the front \
      \while it does, which is more than the threshold, so banding Y alone \
      \puts bl45 and bl46 one band ahead of bl47 and bl44 and reads them \
      \bl45 bl46 bl47 bl44. Holding a column back to the band its lowest \
      \node landed in gives bl47 bl44 bl45 bl46, which is what was decided."
    order <- orderWith (SortAxes AxisY AxisZ AxisX) 0.23 bodyFixture
    let names = group "bl" order
    drop (length names - 4) names `shouldBe` ["bl47", "bl44", "bl45", "bl46"]

  it "leaves the body's middle nodes alone" $ do
    order <- orderWith (SortAxes AxisY AxisZ AxisX) 0.23 bodyFixture
    group "bmm" order `shouldBe` ["bmm0", "bmm1", "bmm2", "bmm3", "bmm4"]

  it "reads the middle group by X as written, not by its distance from the centre" $ do
    order <- orderWith (SortAxes AxisY AxisZ AxisX) 0.05 middleGroupFixture
    order `shouldBe` ["nl0", "nm0", "nm1", "nr0"]

  it "reads a part with no beams in bands, since it has no chains to read" $ do
    order <- orderWith defaultSortAxes 0.05 noBeamsFixture
    order `shouldBe` ["nl0", "nl1", "nr2", "nr3"]

{- | The names and coordinates of what a transform wrote out, and of what a
second transform over that output wrote out. The names are part of it because
the defect this catches renames a vertex without moving it.
-}
namesTwice
  :: SortAxes
  -> Scientific
  -> FilePath
  -> IO
       ( [(Text, (Double, Double, Double))]
       , [(Text, (Double, Double, Double))]
       )
namesTwice axes threshold path = do
  topNode <- parseJbeamFile path
  let config =
        newTransformationConfig
          { sortAxes = axes
          , ySortingThreshold = threshold
          }
      run node =
        case transform M.empty config node of
          Left err -> fail ("transform failed: " ++ T.unpack err)
          Right (_, _, _, resultNode) -> pure resultNode
  onceNode <- run topNode
  twiceNode <- run onceNode
  pure (vertexNamesInOrder onceNode, vertexNamesInOrder twiceNode)

{- | Running the tool over a directory twice is ordinary use, and a part that
comes out differently the second time has no settled order to mark up at all.
The axes and thresholds are the ones each part asks for in `columnOrderSpec`,
so these say the wanted order is reachable as well as right.
-}
partFixedPointSpec :: Spec
partFixedPointSpec = describe "transforming a part a second time" $ do
  it "writes the hood out exactly as the first transform left it" $ do
    (once, twice) <- namesTwice (SortAxes AxisX AxisY AxisZ) 0.23 hoodFixture
    once `shouldNotBe` []
    twice `shouldBe` once

  it "writes the front bumper out exactly as the first transform left it" $ do
    (once, twice) <- namesTwice (SortAxes AxisX AxisZ AxisY) 0.23 frontBumperFixture
    once `shouldNotBe` []
    twice `shouldBe` once

  it "writes the body out exactly as the first transform left it" $ do
    pendingWith
      "A second transform renames bl0 to bll0 and br0 to brr0, and the first \
      \vertex of each side already has that name, so the body ends up with \
      \two bll0 and two brr0. Those four names are the only thing the second \
      \run changes here. The body has no settled order to mark up until a \
      \rename leaves the names unique."
    (once, twice) <- namesTwice (SortAxes AxisY AxisZ AxisX) 0.23 bodyFixture
    once `shouldNotBe` []
    twice `shouldBe` once
