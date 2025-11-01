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

typeForNames :: Map VertexTreeType [Vertex] -> Map Text VertexTreeType
typeForNames groupedVs = M.fromList [(vName v, t) | (t, vs) <- M.toList groupedVs, v <- vs]

vertexConns
  :: Node
  -> Map VertexTreeType [Vertex]
  -> Either Text ([Node], VertexConnMap)
vertexConns topNode vsPerType = case NP.queryNodes beamQuery topNode of
  Just (Array beams) -> Right $ go beams
  _ -> Left $ "could not find " <> show beamQuery
  where
    go :: Vector Node -> ([Node], VertexConnMap)
    go beams =
      let (badNodes, beamNames) = mapResult possiblyBeam beams
          typeMap = typeForNames vsPerType
          fun vtype (beam1, beam2) = increaseBeamCount vtype beam1 beam2 . increaseBeamCount vtype beam2 beam1
          increaseBeamCount vtype beamName otherBeam beamAcc =
            if M.lookup otherBeam typeMap == Just vtype
              then
                M.insertWith (+) beamName 1 beamAcc
              else
                beamAcc
          connCountPerType vtype _ = M.insert vtype (foldr (fun vtype) M.empty beamNames)
       in (badNodes, M.foldrWithKey connCountPerType M.empty vsPerType)
