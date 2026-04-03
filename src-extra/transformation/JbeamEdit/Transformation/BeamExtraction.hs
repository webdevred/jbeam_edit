module JbeamEdit.Transformation.BeamExtraction (vertexConns, possiblyBeam, extractBeams, extractBeamsWithMeta, beamInKnownSet) where

import Data.List (genericTake, sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.IsList
import JbeamEdit.Core.Node
import JbeamEdit.Core.NodePath qualified as NP
import JbeamEdit.Transformation.Types
import JbeamEdit.Transformation.VertexExtraction (metaMapFromObject)
import Numeric.Natural (Natural)

beamQuery :: NP.NodePath
beamQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "beams"]

possiblyBeam :: MetaMap -> Node -> Either Node (Maybe Beam)
possiblyBeam sectionMeta node
  | isCommentNode node || isObjectNode node = Right Nothing
  | otherwise = case node of
      Array av ->
        Right (extractBeamFromArray sectionMeta (avElements av))
      _ -> Left node

extractBeamFromArray :: MetaMap -> Vector Node -> Maybe Beam
extractBeamFromArray sectionMeta vec
  | V.length vec < 2 = Nothing
  | otherwise = do
      n1 <- maybeString (vec V.! 0)
      n2 <- maybeString (vec V.! 1)
      if T.isSuffixOf ":" n1
        then Nothing
        else
          let inlineObjects = mapMaybe maybeObject (V.toList (V.drop 2 vec))
              inlineMeta = M.unions (map metaMapFromObject inlineObjects)
              effectiveMeta = M.union inlineMeta sectionMeta
           in Just (Beam (mkBeamPair n1 n2) effectiveMeta)
  where
    maybeString (String t) = Just t
    maybeString _ = Nothing
    maybeObject n@(Object _) = Just n
    maybeObject _ = Nothing

extractBeams :: Node -> Either Text (Vector Node)
extractBeams topNode =
  NP.queryNodes beamQuery topNode
    >>= NP.expectArray beamQuery

extractBeamsWithMeta :: Vector Node -> ([Node], [Beam])
extractBeamsWithMeta = go M.empty [] [] . V.toList
  where
    go _meta badAcc beamAcc [] = (reverse badAcc, reverse beamAcc)
    go meta badAcc beamAcc (n : ns) =
      case n of
        Object _ ->
          let newMeta = M.union (metaMapFromObject n) meta
           in go newMeta badAcc beamAcc ns
        _ ->
          case possiblyBeam meta n of
            Left bad -> go meta (bad : badAcc) beamAcc ns
            Right Nothing -> go meta badAcc beamAcc ns
            Right (Just beam) -> go meta badAcc (beam : beamAcc) ns

vertexConns
  :: Natural
  -> Node
  -> Map VertexTreeType [AnnotatedVertex]
  -> Either Text ([Node], VertexConnMap)
vertexConns maxSupport topNode vsPerType =
  go <$> extractBeams topNode
  where
    knownNodeNames = S.fromList $ concatMap (map anVertexName) vsPerType
    go beamNodes =
      let (badNodes, beams) = extractBeamsWithMeta beamNodes
          validBeams = filter (beamInKnownSet knownNodeNames) beams

          counts :: Map Text Int
          counts = foldr countBeamNodes M.empty validBeams

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

beamInKnownSet :: Set Text -> Beam -> Bool
beamInKnownSet known (Beam (BeamPair a b) _) =
  S.member a known && S.member b known

countBeamNodes :: Beam -> Map Text Int -> Map Text Int
countBeamNodes (Beam (BeamPair a b) _) =
  M.insertWith (+) a 1 . M.insertWith (+) b 1
