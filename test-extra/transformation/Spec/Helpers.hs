-- | Ways of reading a transformed top node back out, shared by the specs.
module Spec.Helpers (
  parseJbeamFile,
  vertexPositionsInOrder,
  vertexCoordinatesInOrder,
  vertexTextsInOrder,
  vertexCoordinates,
  effectiveMetaByCoordinate,
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
import JbeamEdit.Core.Node (
  Node (..),
  NumberValue (..),
  expectArray,
 )
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

{- | Every vertex coordinate in a top node's "nodes" section, in the order
`transform` wrote them out. Use this where the defect is about which vertex
ended up where, rather than about which vertices survived.
-}
vertexCoordinatesInOrder :: Node -> [(Double, Double, Double)]
vertexCoordinatesInOrder topNode =
  case NP.queryNodes nodesQuery topNode >>= NP.expectArray nodesQuery of
    Left _ -> []
    Right rows ->
      [ (realToFrac (nvValue x), realToFrac (nvValue y), realToFrac (nvValue z))
      | row <- V.toList rows
      , Just inner <- [expectArray row]
      , Just (String name) <- [inner V.!? 0]
      , name /= "id"
      , Just (Number x) <- [inner V.!? 1]
      , Just (Number y) <- [inner V.!? 2]
      , Just (Number z) <- [inner V.!? 3]
      ]

{- | The coordinates as text rather than as numbers, so a spec can say what
a transform wrote back rather than what it means.
-}
vertexTextsInOrder :: Node -> [(Text, Text, Text)]
vertexTextsInOrder topNode =
  case NP.queryNodes nodesQuery topNode >>= NP.expectArray nodesQuery of
    Left _ -> []
    Right rows ->
      [ (nvText x, nvText y, nvText z)
      | row <- V.toList rows
      , Just inner <- [expectArray row]
      , Just (String name) <- [inner V.!? 0]
      , name /= "id"
      , Just (Number x) <- [inner V.!? 1]
      , Just (Number y) <- [inner V.!? 2]
      , Just (Number z) <- [inner V.!? 3]
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

{- | What each vertex in a "nodes" section actually carries, keyed by
position so it can be compared across a transform that renames and reorders.

jbeam metadata is sticky: a bare object sets properties on every row after it
until a later row overrides the same key, and an object on the vertex row
itself overrides the section value for that one vertex. This replays both the
way the game reads them back, rather than trusting how many rows the tool chose
to emit, so it holds whatever the output looks like.
-}
effectiveMetaByCoordinate :: Node -> M.Map (Double, Double, Double) MetaMap
effectiveMetaByCoordinate topNode =
  case NP.queryNodes nodesQuery topNode >>= NP.expectArray nodesQuery of
    Left _ -> M.empty
    Right rows -> go M.empty M.empty (V.toList rows)
  where
    go _ found [] = found
    go sticky found (row : rest) =
      case vertexRow row of
        Just (pos, inline) ->
          go sticky (M.insert pos (M.union inline sticky) found) rest
        Nothing -> go (M.union (metaMapFromObject row) sticky) found rest

    vertexRow row = do
      inner <- expectArray row
      String name <- inner V.!? 0
      Number x <- inner V.!? 1
      Number y <- inner V.!? 2
      Number z <- inner V.!? 3
      if name == "id"
        then Nothing
        else
          Just
            ( (realToFrac (nvValue x), realToFrac (nvValue y), realToFrac (nvValue z))
            , maybe M.empty metaMapFromObject (inner V.!? 4)
            )

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
