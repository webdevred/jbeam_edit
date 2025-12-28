module JbeamEdit.Transformation.BeamExtraction (vertexConns) where

import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List (genericTake, sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Vector qualified as V
import GHC.IsList
import JbeamEdit.Core.Node
import JbeamEdit.Core.NodePath qualified as NP
import JbeamEdit.Transformation.Types
import Numeric.Natural (Natural)

beamQuery :: NP.NodePath
beamQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "beams"]

rejectUnknownName
  :: Foldable t
  => t Text
  -> Maybe (Text, Text)
  -> Maybe (Text, Text)
rejectUnknownName knownNodeNames maybeBeam =
  bool
    maybeBeam
    Nothing
    (any (uncurry ((||) `on` (`notElem` knownNodeNames))) maybeBeam)

possiblyBeam :: Node -> Either Node (Maybe (Text, Text))
possiblyBeam node
  | isCommentNode node || isObjectNode node = Right Nothing
  | otherwise = maybe (Left node) (Right . Just) maybeBeam
  where
    maybeBeam =
      case node of
        (Array beamVec) ->
          case V.toList beamVec of
            (String vertex1 : String vertex2 : _) -> Just (vertex1, vertex2)
            _ -> Nothing
        _ -> Nothing

vertexConns
  :: Natural
  -> Node
  -> Map VertexTreeType [AnnotatedVertex]
  -> Either Text ([Node], VertexConnMap)
vertexConns maxSupport topNode vsPerType =
  NP.queryNodes beamQuery topNode
    >>= NP.expectArray beamQuery
    <&> go
  where
    knownNodeNames = concatMap (map (vName . aVertex)) vsPerType
    go beams =
      let possiblyInnerBeam = (:) . fmap (rejectUnknownName knownNodeNames) . possiblyBeam
          (badNodes, beamPairs) =
            partitionEithers (V.foldr possiblyInnerBeam [] beams)

          counts :: Map Text Int
          counts =
            foldr
              ( \(a, b) acc ->
                  acc
                    & M.insertWith (+) a 1
                    & M.insertWith (+) b 1
              )
              M.empty
              (catMaybes beamPairs)

          topVerticesPerType :: Map VertexTreeType [(AnnotatedVertex, Int)]
          topVerticesPerType =
            M.map
              ( \vs ->
                  genericTake maxSupport
                    . sortOn
                      (Down . snd)
                    $ ([(v, M.findWithDefault 0 (vName $ aVertex v) counts) | v <- vs])
              )
              vsPerType

          vertexConnMap :: VertexConnMap
          vertexConnMap =
            M.fromList
              [ (vName $ aVertex v, (t, c))
              | (t, vs) <- M.toList topVerticesPerType
              , (v, c) <- vs
              ]
       in (badNodes, vertexConnMap)
