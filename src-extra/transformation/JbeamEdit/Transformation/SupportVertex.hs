module JbeamEdit.Transformation.SupportVertex (vertexConns) where

import Core.Node
import Core.NodePath qualified as NP
import Core.Result
import Data.Map qualified as M
import Data.Vector qualified as V
import JbeamEdit.Transformation.Types

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

vertexConns
  :: Int
  -> Node
  -> Map VertexTreeType [AnnotatedVertex]
  -> Either Text ([Node], VertexConnMap)
vertexConns maxX topNode vsPerType = case NP.queryNodes beamQuery topNode of
  Just (Array beams) -> Right $ go beams
  _ -> Left $ "could not find " <> show beamQuery
  where
    go beams =
      let (badNodes, beamPairs) = mapResult possiblyBeam beams

          counts :: Map Text Int
          counts =
            foldr
              ( \(a, b) acc ->
                  acc
                    & M.insertWith (+) a 1
                    & M.insertWith (+) b 1
              )
              M.empty
              beamPairs

          topVerticesPerType :: Map VertexTreeType [(AnnotatedVertex, Int)]
          topVerticesPerType =
            M.map
              ( \vs ->
                  take
                    maxX
                    ( sortWith
                        (Down . snd)
                        ([(v, M.findWithDefault 0 (vName $ aVertex v) counts) | v <- vs])
                    )
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
