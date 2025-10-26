module SupportVertex (vertexConns) where

import Core.Node
import Core.NodePath qualified as NP
import Core.Result
import Data.Map qualified as M
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
          case toList beamVec of
            [String vertex1, String vertex2] -> Just (vertex1, vertex2)
            _ -> Nothing
        _ -> Nothing

vertexConns :: Node -> Either Text ([Node], VertexConnMap)
vertexConns topNode = case NP.queryNodes beamQuery topNode of
  Just (Array beams) -> Right $ go beams
  _ -> Left $ "could not find " <> show beamQuery
  where
    go :: Seq Node -> ([Node], VertexConnMap)
    go beams =
      let (badNodes, beamNames) = mapResult possiblyBeam beams
          fun (beam1, beam2) = increaseBeamCount beam1 . increaseBeamCount beam2
          increaseBeamCount beamName = M.insertWith (+) beamName 1
          connCountMap = foldr fun M.empty beamNames
       in (badNodes, connCountMap)
