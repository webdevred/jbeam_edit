-- | Ways of reading a transformed top node back out, shared by the specs.
module Spec.Helpers (
  parseJbeamFile,
  vertexPositionsInOrder,
  vertexCoordinates,
  effectiveMetaAtFirstVertex,
  metaNumber,
  outOfOrderPairs,
) where

import Data.Char (isDigit)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.IsList (fromList)
import JbeamEdit.Core.Node (Node (..), NumberValue (..), expectArray)
import JbeamEdit.Core.NodePath qualified as NP
import JbeamEdit.IOUtils (tryReadFile)
import JbeamEdit.Parsing.Jbeam (parseNodes)
import JbeamEdit.Transformation.Types (MetaMap)
import JbeamEdit.Transformation.VertexExtraction (metaMapFromObject)
import System.OsPath

parseJbeamFile :: FilePath -> IO Node
parseJbeamFile path = do
  let osPath = unsafeEncodeUtf path
  contents <- tryReadFile [] osPath
  case contents >>= parseNodes of
    Right node -> pure node
    Left err -> fail $ "Failed to parse " ++ path ++ ": " ++ T.unpack err

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

{- | The metadata the first vertex in a transformed file actually carries.
jbeam metadata is sticky and a later row overrides an earlier one, so this
replays the rows ahead of that vertex the way the game reads them back rather
than trusting how many rows the tool chose to emit.
-}
effectiveMetaAtFirstVertex :: Node -> MetaMap
effectiveMetaAtFirstVertex topNode =
  case NP.queryNodes nodesQuery topNode >>= NP.expectArray nodesQuery of
    Left _ -> M.empty
    Right rows -> go M.empty (V.toList rows)
  where
    go acc [] = acc
    go acc (row : rest)
      | isVertexRow row = acc
      | otherwise = go (M.union (metaMapFromObject row) acc) rest
    isVertexRow row =
      case expectArray row >>= (V.!? 0) of
        Just (String name) -> name /= "id"
        _ -> False

metaNumber :: Text -> MetaMap -> Maybe Double
metaNumber key meta =
  case M.lookup key meta of
    Just (Number n) -> Just (realToFrac (nvValue n))
    _ -> Nothing

{- | Y positions of vertex pairs a transform wrote out of order: a later
vertex sitting more than the threshold further forward than an earlier
one in the same output group. Only vertices within one group are
compared (e.g. "nll", "nlm"): a Left-tree vertex and a Middle-tree
vertex are unrelated blocks in the file, not a single ordered sequence,
so their relative position isn't meaningful.
-}
outOfOrderPairs :: Double -> Node -> [(Double, Double)]
outOfOrderPairs thr resultNode =
  [ (y1, y2)
  | ((n1, y1), i1) <- positions
  , ((n2, y2), i2) <- positions
  , i1 < i2
  , groupPrefix n1 == groupPrefix n2
  , y1 - y2 > thr
  ]
  where
    positions = zip (vertexPositionsInOrder resultNode) [0 :: Int ..]
    groupPrefix = T.dropWhileEnd isDigit
