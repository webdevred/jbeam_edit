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

noBeamsFixture :: FilePath
noBeamsFixture = "examples/regression_jbeam/no-beams-repro.jbeam"

bodyFrontEdgeOrder :: Text -> [Text]
bodyFrontEdgeOrder = bodyOrder [0, 2, 4, 1, 3]

bodyRearColumnOrder :: Text -> [Text]
bodyRearColumnOrder = bodyOrder [47, 44, 45, 46]

-- Nothing sits between the two columns in the fixture.
bodySideOrder :: Text -> [Text]
bodySideOrder prefix = bodyFrontEdgeOrder prefix <> bodyRearColumnOrder prefix

bodyOrder :: [Int] -> Text -> [Text]
bodyOrder indices prefix = map named indices
  where
    named index = prefix <> T.pack (show index)

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
      "No change to the tool turns this one green. The right side holds a \
      \beam between fbr1 and fbm0 where the left side has one between fbl1 \
      \and fbl0, and of the 31 beams here whose two nodes both have a mirror \
      \node, that is the only pair with no mirror image. The order asserted \
      \here needs that beam to have one, so it waits on the file and not on \
      \the code, and the fixture is left exactly as it was sent. What the \
      \tool is missing meanwhile is only the columns from the beams: the \
      \side reads fbr3 fbr0 fbr1 fbr2 fbr4 fbr5 fbr6 fbr7 today, and its X \
      \values are negative, so comparing X as written already puts the outer \
      \column first here."
    order <- orderWith (SortAxes AxisX AxisZ AxisY) 0.23 frontBumperFixture
    group "fbr" order
      `shouldBe` ["fbr0", "fbr1", "fbr2", "fbr3", "fbr4", "fbr5", "fbr6", "fbr7"]

  it "reads the body's front edge as two columns" $ do
    pendingWith
      "The front edge leans: its first column runs 0.152 along Y as it rises \
      \and the next one starts only 0.008 behind where that column ends. All \
      \five nodes share one band at 0.23 and Z then reads them bl0 bl1 bl2 \
      \bl3 bl4, which is the order the file already has and still not the \
      \order asked for. A threshold of 0.16 does separate the two columns, \
      \and it leaves the rear column below reading bl45 bl46 bl47 bl44, so \
      \no one threshold serves both. Taking the columns from the beams gives \
      \bl0 bl2 bl4 bl1 bl3. The rear column of the same side is the spec \
      \below, which asks for the hold-back instead."
    order <- orderWith (SortAxes AxisY AxisZ AxisX) 0.23 bodyFixture
    take 5 (group "bl" order) `shouldBe` bodyFrontEdgeOrder "bl"

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
    drop (length names - 4) names `shouldBe` bodyRearColumnOrder "bl"

  it "leaves the body's middle nodes alone" $ do
    order <- orderWith (SortAxes AxisY AxisZ AxisX) 0.23 bodyFixture
    group "bmm" order `shouldBe` ["bmm0", "bmm1", "bmm2", "bmm3", "bmm4"]

  it "reads the middle group by X as written, not by its distance from the centre" $ do
    order <- orderWith (SortAxes AxisY AxisZ AxisX) 0.05 middleGroupFixture
    order `shouldBe` ["nl0", "nm0", "nm1", "nr0"]

  it "reads a part with no beams in bands, since it has no chains to read" $ do
    order <- orderWith defaultSortAxes 0.05 noBeamsFixture
    order `shouldBe` ["nl0", "nl1", "nr2", "nr3"]

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

-- Uses the axes and thresholds of columnOrderSpec.
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
    (once, twice) <- namesTwice (SortAxes AxisY AxisZ AxisX) 0.23 bodyFixture
    once `shouldNotBe` []
    twice `shouldBe` once
