module SupportVertex (vertexConns) where

import Core.Node
import Core.NodePath qualified as NP
import Core.Result
import Data.Map qualified as M
import Data.Vector (Vector)
import Data.Vector qualified as V
import Types

beamQuery :: NP.NodePath
beamQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "beams"]

possiblyBeam :: Node -> Result Node (Text, Text)
possiblyBeam node
  | isCommentNode node || isObjectNode node = Empty
  | otherwise = maybe (Bad node) Good maybeBeam
  where
    maybeBeam =
      case node of
        (Array beamVec) ->
          case V.toList beamVec of
            [String vertex1, String vertex2] -> Just (vertex1, vertex2)
            _ -> Nothing
        _ -> Nothing

typeForNames :: Map VertexTreeType [AnnotatedVertex] -> Map Text VertexTreeType
typeForNames groupedVs = M.fromList [(vName $ aVertex v, t) | (t, vs) <- M.toList groupedVs, v <- vs]

vertexConns
  :: Node
  -> Map VertexTreeType [AnnotatedVertex]
  -> Either Text ([Node], VertexConnMap)
vertexConns topNode vsPerType = case NP.queryNodes beamQuery topNode of
  Just (Array beams) -> Right $ go beams
  _ -> Left $ "could not find " <> show beamQuery
  where
    typeMap :: Map Text VertexTreeType
    typeMap = typeForNames vsPerType

    updateConn :: VertexTreeType -> Text -> VertexConnMap -> VertexConnMap
    updateConn vtype key =
      M.insertWith
        (M.unionWith (+))
        vtype
        (M.singleton key 1)

    go :: Vector Node -> ([Node], VertexConnMap)
    go beams =
      let (badNodes, beamPairs) = mapResult possiblyBeam beams
          connMap = foldr updateForPair M.empty beamPairs
       in (badNodes, connMap)

    updateForPair :: (Text, Text) -> VertexConnMap -> VertexConnMap
    updateForPair (beam1, beam2) acc =
      let acc' = case M.lookup beam2 typeMap of
            Just t -> updateConn t beam1 acc
            Nothing -> acc
          acc'' = case M.lookup beam1 typeMap of
            Just t' -> updateConn t' beam2 acc'
            Nothing -> acc'
       in acc''
