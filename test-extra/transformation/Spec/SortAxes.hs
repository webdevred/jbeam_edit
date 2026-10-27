{- | One grid read twice, once with the axes the tool has always used and once
with two of them swapped. Between them the two specs cover all three axes: the
grouping and walking axes decide the second, and since every Z in the grid is
equal, the tie axis alone decides the first.
-}
module Spec.SortAxes (
  sortAxesSpec,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import JbeamEdit.Core.Node (Node)
import JbeamEdit.Parsing.Jbeam (parseNodes)
import JbeamEdit.Transformation
import JbeamEdit.Transformation.Config
import JbeamEdit.Transformation.Types (Axis (..), SortAxes (..))
import Spec.Helpers (vertexCoordinatesInOrder)
import Test.Hspec

{- | Two columns of three, all at one height, so the walk axis alone decides
the order inside a band and the tie axis never speaks. The rows are written in
neither of the two orders the specs expect, so a sort that ignored an axis and
left the input order standing would fail both. The beam is there because the
transform needs a beams section, and one connection is far below the support
threshold for six vertices.
-}
gridSource :: Text
gridSource =
  T.unlines
    [ "{\"testpart\":{"
    , "    \"nodes\":["
    , "        [\"id\", \"posX\", \"posY\", \"posZ\"],"
    , "        [\"nl0\", 0.3,  1.0, 0.5],"
    , "        [\"nl1\", 0.8,  0.0, 0.5],"
    , "        [\"nl2\", 0.3, -1.0, 0.5],"
    , "        [\"nl3\", 0.8,  1.0, 0.5],"
    , "        [\"nl4\", 0.3,  0.0, 0.5],"
    , "        [\"nl5\", 0.8, -1.0, 0.5],"
    , "    ],"
    , "    \"beams\":["
    , "        [\"id1:\", \"id2:\"],"
    , "        [\"nl0\", \"nl1\"],"
    , "    ],"
    , "},"
    , "}"
    ]

grid :: Node
grid = case parseNodes (LBS.fromStrict (encodeUtf8 gridSource)) of
  Right node -> node
  Left err -> error ("the grid source does not parse: " ++ T.unpack err)

{- | The same grid inside the middle breakpoints, at X -0.02 and 0.06. The two
columns differ in distance from the centre line, so a key that read X as a
distance would put 0.06 first, where the middle group reads X as written.
-}
middleGridSource :: Text
middleGridSource =
  T.unlines
    [ "{\"testpart\":{"
    , "    \"nodes\":["
    , "        [\"id\", \"posX\", \"posY\", \"posZ\"],"
    , "        [\"nm0\", -0.02,  1.0, 0.5],"
    , "        [\"nm1\",  0.06,  0.0, 0.5],"
    , "        [\"nm2\", -0.02, -1.0, 0.5],"
    , "        [\"nm3\",  0.06,  1.0, 0.5],"
    , "        [\"nm4\", -0.02,  0.0, 0.5],"
    , "        [\"nm5\",  0.06, -1.0, 0.5],"
    , "    ],"
    , "    \"beams\":["
    , "        [\"id1:\", \"id2:\"],"
    , "        [\"nm0\", \"nm1\"],"
    , "    ],"
    , "},"
    , "}"
    ]

middleGrid :: Node
middleGrid = case parseNodes (LBS.fromStrict (encodeUtf8 middleGridSource)) of
  Right node -> node
  Left err -> error ("the middle grid source does not parse: " ++ T.unpack err)

coordinatesOf :: Node -> SortAxes -> IO [(Double, Double, Double)]
coordinatesOf node axes =
  case transform M.empty newTransformationConfig {sortAxes = axes} node of
    Left err -> fail ("transform failed: " ++ T.unpack err)
    Right (_, _, _, resultNode) -> pure (vertexCoordinatesInOrder resultNode)

coordinatesWith :: SortAxes -> IO [(Double, Double, Double)]
coordinatesWith = coordinatesOf grid

sortAxesSpec :: Spec
sortAxesSpec = describe "the axes the sort reads" $ do
  it "walks each band across the car with the axes the tool has always used" $
    coordinatesWith defaultSortAxes
      `shouldReturn` [ (0.8, -1.0, 0.5)
                     , (0.3, -1.0, 0.5)
                     , (0.8, 0.0, 0.5)
                     , (0.3, 0.0, 0.5)
                     , (0.8, 1.0, 0.5)
                     , (0.3, 1.0, 0.5)
                     ]

  it "walks each column front to back when the grouping and walking axes swap" $
    coordinatesWith (SortAxes AxisX AxisY AxisZ)
      `shouldReturn` [ (0.8, -1.0, 0.5)
                     , (0.8, 0.0, 0.5)
                     , (0.8, 1.0, 0.5)
                     , (0.3, -1.0, 0.5)
                     , (0.3, 0.0, 0.5)
                     , (0.3, 1.0, 0.5)
                     ]

  it "walks each band across the car when X is the walking axis" $
    coordinatesWith (SortAxes AxisY AxisX AxisZ)
      `shouldReturn` [ (0.8, -1.0, 0.5)
                     , (0.3, -1.0, 0.5)
                     , (0.8, 0.0, 0.5)
                     , (0.3, 0.0, 0.5)
                     , (0.8, 1.0, 0.5)
                     , (0.3, 1.0, 0.5)
                     ]

  it "reads the middle group by X as written" $
    coordinatesOf middleGrid defaultSortAxes
      `shouldReturn` [ (-0.02, -1.0, 0.5)
                     , (0.06, -1.0, 0.5)
                     , (-0.02, 0.0, 0.5)
                     , (0.06, 0.0, 0.5)
                     , (-0.02, 1.0, 0.5)
                     , (0.06, 1.0, 0.5)
                     ]
