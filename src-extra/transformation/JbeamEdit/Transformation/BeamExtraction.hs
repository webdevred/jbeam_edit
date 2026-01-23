module JbeamEdit.Transformation.BeamExtraction (vertexConns, possiblyBeam, extractBeams) where

import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.List (genericTake, sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Ord (Down (Down))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.IsList
import JbeamEdit.Core.Node
import JbeamEdit.Core.NodePath qualified as NP
import JbeamEdit.Transformation.Types
import Numeric.Natural (Natural)

beamQuery :: NP.NodePath
beamQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "beams"]

rejectUnknownName
  :: Set Text
  -> Maybe (Vector Text)
  -> Maybe (Vector Text)
rejectUnknownName knownNodeNames maybeBeam =
  bool
    maybeBeam
    Nothing
    (any (any (`S.notMember` knownNodeNames)) maybeBeam)

possiblyBeam :: Node -> Either Node (Maybe (Vector Text))
possiblyBeam node
  | isCommentNode node || isObjectNode node = Right Nothing
  | otherwise = maybe (Left node) (Right . Just) (maybeBeam node)
  where
    maybeString (String vertexName) = Just vertexName
    maybeString _ = Nothing
    maybeBeam (Array beamVec) = mapM maybeString beamVec
    maybeBeam _ = Nothing

extractBeams :: Node -> Either Text (Vector Node)
extractBeams topNode =
  NP.queryNodes beamQuery topNode
    >>= NP.expectArray beamQuery

vertexConns
  :: Natural
  -> Node
  -> Map VertexTreeType [AnnotatedVertex]
  -> Either Text ([Node], VertexConnMap)
vertexConns maxSupport topNode vsPerType =
  go <$> extractBeams topNode
  where
    knownNodeNames = foldMap (foldr (S.insert . anVertexName) mempty) vsPerType
    go beams =
      let possiblyInnerBeam = (:) . fmap (rejectUnknownName knownNodeNames) . possiblyBeam
          (badNodes, beamPairs) =
            partitionEithers (V.foldr possiblyInnerBeam [] beams)

          counts :: Map Text Int
          counts =
            foldr
              (flip (V.foldr (\nodeName -> M.insertWith (+) nodeName 1)))
              M.empty
              (catMaybes beamPairs)

          topVerticesPerType :: Map VertexTreeType [(AnnotatedVertex, Int)]
          topVerticesPerType =
            M.map
              ( \vs ->
                  genericTake maxSupport
                    . sortOn
                      (Down . snd)
                    $ ([(v, M.findWithDefault 0 (anVertexName v) counts) | v <- vs])
              )
              vsPerType

          vertexConnMap :: VertexConnMap
          vertexConnMap =
            M.fromList
              [ (anVertexName v, (t, c))
              | (t, vs) <- M.toList topVerticesPerType
              , (v, c) <- vs
              ]
       in (badNodes, vertexConnMap)
