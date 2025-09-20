module SupportVertex (vertexConns) where

import Core.Node
import Data.Vector (Vector)
import Types

import Core.NodePath qualified as NP
import Data.Map qualified as M
import Data.Vector qualified as V

beamQuery :: NP.NodePath
beamQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "beams"]

possiblyBeam :: Node -> Maybe (Text, Text)
possiblyBeam node
  | isJust maybeBeam = maybeBeam
  | isCommentNode node = Nothing
  | otherwise = Nothing
  where
    maybeBeam =
      case node of
        (Array beamVec) ->
          case V.toList beamVec of
            [String vertex1, String vertex2] -> Just (vertex1, vertex2)
            _ -> Nothing
        _ -> Nothing

vertexConns :: Node -> Either Text VertexConnMap
vertexConns topNode = case NP.queryNodes beamQuery topNode of
  Just (Array beams) -> Right $ go beams
  _ -> Left $ "could not find " <> show beamQuery
  where
    go :: Vector Node -> VertexConnMap
    go beams =
      let beamNames = V.mapMaybe possiblyBeam beams
          fun (beam1, beam2) = increaseBeamCount beam1 . increaseBeamCount beam2
          increaseBeamCount beamName = M.insertWith (+) beamName 1
       in V.foldr fun M.empty beamNames
