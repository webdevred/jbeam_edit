module VertexExtraction (
  getVertexForest,
  determineGroup,
  determineGroup',
  combineTrees,
  metaMapFromObject,
  dropIndex,
) where

import Config
import Core.Node
import Data.Char (isDigit)
import Data.Semigroup.Foldable (fold1)
import Data.Vector (Vector)
import Data.Vector.NonEmpty (NonEmptyVector (..))
import Types

import Core.NodePath qualified as NP
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.NonEmpty qualified as NEV

newVertex :: Node -> Maybe Vertex
newVertex (Array ns) = f . V.toList $ ns
  where
    f [String name, Number x, Number y, Number z, Object m] =
      Just (Vertex {vName = name, vX = x, vY = y, vZ = z, vMeta = Just m})
    f [String name, Number x, Number y, Number z] =
      Just (Vertex {vName = name, vX = x, vY = y, vZ = z, vMeta = Nothing})
    f _ = Nothing
newVertex _ = Nothing

isNonVertex :: Node -> Bool
isNonVertex = isNothing . newVertex

dropIndex :: Text -> Text
dropIndex = T.dropWhileEnd isDigit

hasVertexPrefix :: Maybe Text -> Node -> Bool
hasVertexPrefix vertexPrefix1 node =
  let vertexPrefix2 = dropIndex <$> getVertexName node
   in vertexPrefix1 == vertexPrefix2

getVertexName :: Node -> Maybe Text
getVertexName = fmap vName . newVertex

getFirstVertexName :: Vector Node -> Maybe Text
getFirstVertexName nodes = do
  (node, _) <- V.uncons nodes
  getVertexName node

getVertexPrefix :: Vector Node -> Maybe Text
getVertexPrefix nodes = do
  firstVertexName <- getFirstVertexName nodes
  (_, vertexIndex) <- T.unsnoc firstVertexName
  guard $ isDigit vertexIndex
  let vertexPrefix = dropIndex firstVertexName
  guard . not . T.null $ vertexPrefix
  pure vertexPrefix

isCollision :: Node -> Set Text -> Either Text (Set Text)
isCollision vertexNode vertexNames =
  case getVertexName vertexNode of
    Just vertexName ->
      if S.member vertexName vertexNames
        then Left $ "multiple vertices named " <> vertexName
        else Right (S.insert vertexName vertexNames)
    Nothing -> Right vertexNames

breakVertices
  :: Maybe Text
  -> Set Text
  -> V.Vector Node
  -> Either Text (Set Text, Vector Node, Vector Node)
breakVertices vertexPrefix allVertexNames ns = go V.empty ns allVertexNames
  where
    go acc vns vertexNames
      | V.null vns = Right (vertexNames, V.reverse acc, V.empty)
      | otherwise =
          let node = V.head vns
              rest = V.tail vns
              maybeVertex = newVertex node
           in case maybeVertex of
                Nothing -> go (V.cons node acc) rest vertexNames
                Just _
                  | hasVertexPrefix vertexPrefix node
                      || (isNothing vertexPrefix && any isSupportVertex maybeVertex) ->
                      isCollision node vertexNames >>= go (V.cons node acc) rest
                  | otherwise ->
                      let (metaBefore, currentTree) = V.span isNonVertex acc
                       in if V.null currentTree
                            then Right (vertexNames, V.singleton node, metaBefore <> rest)
                            else
                              Right
                                (vertexNames, V.reverse currentTree, V.reverse metaBefore <> V.cons node rest)

combineTrees :: VertexTree -> VertexTree -> VertexTree
combineTrees (VertexTree _ newVertexGroups) (VertexTree oldComments oldVertexGroups) =
  VertexTree
    { tComments = oldComments
    , tAnnotatedVertices = oldVertexGroups <> newVertexGroups
    }

isSupportVertex :: Vertex -> Bool
isSupportVertex v =
  case T.unsnoc (vName v) of
    Nothing -> True
    Just (_, c) -> not (isDigit c)

metaMapFromObject :: Node -> MetaMap
metaMapFromObject (Object objKeys) =
  let toKV (ObjectKey (String k, v)) = Just (k, v)
      toKV _ = Nothing
   in M.fromList . mapMaybe toKV $ V.toList objKeys
metaMapFromObject _ = M.empty

toInternalComment :: Node -> Maybe InternalComment
toInternalComment (Comment c) = Just c
toInternalComment _ = Nothing

addCommentToAn
  :: InternalComment
  -> AnnotatedVertex
  -> AnnotatedVertex
addCommentToAn ic (AnnotatedVertex comments vertex meta) = AnnotatedVertex (V.singleton ic <> comments) vertex meta

nodesToAnnotatedVertices
  :: MetaMap
  -> Vector Node
  -> Either Text (NonEmptyVector AnnotatedVertex)
nodesToAnnotatedVertices initialMeta nodes = go initialMeta [] nodes []
  where
    go pendingMeta pendingComments currentNodes acc
      | V.null currentNodes =
          case NEV.fromVector . V.reverse $ fromList acc of
            Nothing -> Left "no vertices found"
            Just revAcc -> Right revAcc
      | otherwise =
          case V.uncons currentNodes of
            Just (n, ns) ->
              case newVertex n of
                Just v ->
                  let av = AnnotatedVertex (V.reverse $ V.fromList pendingComments) v pendingMeta
                   in go pendingMeta [] ns (av : acc)
                Nothing
                  | isObjectNode n ->
                      let newMeta = M.union (metaMapFromObject n) pendingMeta
                       in go newMeta pendingComments ns acc
                  | isCommentNode n ->
                      case (toInternalComment n, acc) of
                        (Just ic@(InternalComment _ _ PreviousNode), an : ans) ->
                          go pendingMeta pendingComments ns (addCommentToAn ic an : ans)
                        (Just ic, _) ->
                          go pendingMeta (ic : pendingComments) ns acc
                        (Nothing, _) ->
                          go pendingMeta pendingComments ns acc
                  | otherwise -> go pendingMeta pendingComments ns acc

newVertexTree
  :: XGroupBreakpoints
  -> Set Text
  -> VertexForest
  -> NonEmptyVector Node
  -> Either Text (Set Text, VertexTreeType, VertexTree, VertexForest, Vector Node)
newVertexTree brks vertexNames vertexForest nodes =
  let (topNodes, nodes') = NEV.span isNonVertex nodes
      topComments = V.mapMaybe toInternalComment topNodes
      topMeta = M.unions . V.map metaMapFromObject $ topNodes
      vertexPrefix = getVertexPrefix nodes'
   in case breakVertices vertexPrefix vertexNames nodes' of
        Left err -> Left err
        Right (vertexNames', vertexNodes, rest') ->
          case nodesToAnnotatedVertices topMeta vertexNodes of
            Left err -> Left err
            Right avNE ->
              let firstAv = NEV.head avNE
                  vertexTree = VertexTree topComments (NEV.singleton avNE)
               in case determineGroup' brks (aVertex firstAv) of
                    Just treeType ->
                      let updatedForest = M.insertWith combineTrees treeType vertexTree vertexForest
                       in Right (vertexNames', treeType, vertexTree, updatedForest, rest')
                    Nothing -> Left "invalid breakpoint"

determineGroup :: XGroupBreakpoints -> Vertex -> Maybe VertexTreeType
determineGroup (XGroupBreakpoints brks) v =
  case [vtype | (XGroupBreakpoint f brk, vtype) <- brks, applyOperator f (vX v) brk] of
    (vtype : _) -> Just vtype
    [] -> Nothing

determineGroup' :: XGroupBreakpoints -> Vertex -> Maybe VertexTreeType
determineGroup' brks v
  | isSupportVertex v = Just SupportTree
  | otherwise = determineGroup brks v

nodesListToTree
  :: XGroupBreakpoints
  -> NonEmptyVector Node
  -> Either Text (VertexTreeType, VertexForest)
nodesListToTree brks nodes =
  case newVertexTree brks S.empty M.empty nodes of
    Left err -> Left err
    Right (vertexNames, firstTreeType, _firstVertexTree, vertexForest, rest) ->
      case NEV.fromVector rest of
        Nothing -> Right (firstTreeType, vertexForest)
        Just nonEmptyRest -> go vertexNames vertexForest nonEmptyRest firstTreeType
  where
    go vertexNames acc rest firstTreeType =
      case newVertexTree brks vertexNames acc rest of
        Left err -> Left err
        Right (vertexNames', _treeType, _vt, acc', rest') ->
          case NEV.fromVector rest' of
            Nothing -> Right (firstTreeType, acc')
            Just ne -> go vertexNames' acc' ne firstTreeType

objectKeysToObjects :: Map Text Node -> Vector Node
objectKeysToObjects =
  V.fromList
    . map (\(key, value) -> Object . V.singleton $ ObjectKey (String key, value))
    . M.assocs

extractFirstVertex
  :: NonEmptyVector (NonEmptyVector AnnotatedVertex)
  -> (AnnotatedVertex, Vector AnnotatedVertex)
extractFirstVertex groups =
  let (firstGroup, otherGroups) = NEV.uncons groups
      (x, xs) = NEV.uncons firstGroup
      rest = xs <> foldMap NEV.toVector otherGroups
   in (x, rest)

getVertexForestGlobals
  :: Node
  -> (VertexTreeType, VertexForest)
  -> Either Text (NonEmptyVector Node, VertexForest)
getVertexForestGlobals header (treeType, vertexTrees) =
  let firstVertexTree = vertexTrees M.! treeType

      (firstVertex, otherFirstTreeVertices) = extractFirstVertex (tAnnotatedVertices firstVertexTree)

      vertices =
        otherFirstTreeVertices
          <> foldMap (NEV.toVector . fold1 . tAnnotatedVertices) (M.elems vertexTrees)

      isGlobal k v =
        let otherVs = V.map (M.lookup k . aMeta) vertices
         in all (\v' -> isNothing v' || v' == Just v) otherVs
      (globalsMap, localsMap) = M.partitionWithKey isGlobal (aMeta firstVertex)

      setLocals (AnnotatedVertex c v m) = AnnotatedVertex c v (M.union m localsMap)
      updatedForest =
        M.update
          (\(VertexTree c gs) -> Just $ VertexTree c (NEV.map (NEV.map setLocals) gs))
          treeType
          vertexTrees

      globalNodes = objectKeysToObjects globalsMap
   in Right (NEV.consV header globalNodes, updatedForest)

getVertexForest
  :: XGroupBreakpoints
  -> NP.NodePath
  -> Node
  -> Either Text (NonEmptyVector Node, VertexForest)
getVertexForest brks np topNode =
  case NP.queryNodes np topNode of
    Nothing -> Left $ "could not find vertices at path " <> show np
    Just node -> processNode node
  where
    processNode (Array ns)
      | null ns = Left "empty array at vertex path"
      | otherwise =
          case V.uncons ns of
            Just (header, nodesWithoutHeader)
              | isValidVertexHeader header ->
                  case NEV.fromVector nodesWithoutHeader of
                    Nothing -> Left "no nodes after header"
                    Just ne ->
                      case nodesListToTree brks ne of
                        Left err -> Left err
                        Right (firstTreeType, vertexForest) ->
                          getVertexForestGlobals header (firstTreeType, vertexForest)
              | otherwise -> Left "invalid vertex header"
            _ -> Left "missing vertex header"
    processNode bad = Left $ "expected Array at vertex path, got: " <> show bad

isValidVertexHeader :: Node -> Bool
isValidVertexHeader (Array header) =
  V.length header == 4 && all isStringNode header
isValidVertexHeader _ = False
