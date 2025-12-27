module JbeamEdit.Transformation.BeamExtraction (vertexConns) where

import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.Function (on, (&))
import Data.List (genericTake, sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as T
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
  -> Either Node (Maybe (Text, Text))
  -> Either Node (Maybe (Text, Text))
rejectUnknownName knownNodeNames (Right maybeBeam) =
  bool
    (Right maybeBeam)
    (Right Nothing)
    (any (uncurry ((||) `on` (`notElem` knownNodeNames))) maybeBeam)
rejectUnknownName _ beamError = beamError

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
vertexConns maxSupport topNode vsPerType = case NP.queryNodes beamQuery topNode of
  Just (Array beams) -> Right $ go beams
  _ -> Left $ "could not find " <> T.show beamQuery
  where
    knownNodeNames = concatMap (map (vName . aVertex)) vsPerType
    go beams =
      let (badNodes, beamPairs) =
            partitionEithers $
              V.foldr ((:) . rejectUnknownName knownNodeNames . possiblyBeam) [] beams

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
