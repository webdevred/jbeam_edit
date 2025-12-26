module JbeamEdit.Transformation.SupportVertex (vertexConns) where

import Data.Function (on, (&))
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.IsList
import JbeamEdit.Core.Node
import JbeamEdit.Core.NodePath qualified as NP
import JbeamEdit.Core.Result
import JbeamEdit.Transformation.Types

beamQuery :: NP.NodePath
beamQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "beams"]

possiblyBeam :: [Text] -> Node -> Result Node (Text, Text)
possiblyBeam knownNodeNames node
  | hasUnknownNames || isCommentNode node || isObjectNode node = Empty
  | otherwise = maybe (Bad node) Good maybeBeam
  where
    hasUnknownNames = any (uncurry ((||) `on` (`notElem` knownNodeNames))) maybeBeam
    maybeBeam =
      case node of
        (Array beamVec) ->
          case V.toList beamVec of
            (String vertex1 : String vertex2 : _) -> Just (vertex1, vertex2)
            _ -> Nothing
        _ -> Nothing

vertexConns
  :: Int
  -> Node
  -> Map VertexTreeType [AnnotatedVertex]
  -> Either Text ([Node], VertexConnMap)
vertexConns maxX topNode vsPerType = case NP.queryNodes beamQuery topNode of
  Just (Array beams) -> Right $ go beams
  _ -> Left $ "could not find " <> T.show beamQuery
  where
    knownNodeNames = concatMap (map (vName . aVertex)) vsPerType
    go beams =
      let (badNodes, beamPairs) = mapResult (possiblyBeam knownNodeNames) beams

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
                  take maxX $
                    sortOn
                      (Down . snd)
                      ([(v, M.findWithDefault 0 (vName $ aVertex v) counts) | v <- vs])
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
