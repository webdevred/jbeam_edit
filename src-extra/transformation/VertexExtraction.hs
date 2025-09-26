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
import Types

import Core.NodePath qualified as NP
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V

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

getFirstVertexName :: [Node] -> Maybe Text
getFirstVertexName (node : _) = getVertexName node
getFirstVertexName _ = Nothing

getVertexPrefix :: [Node] -> Maybe Text
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
  -> [Node]
  -> Either Text (Set Text, [Node], [Node])
breakVertices vertexPrefix allVertexNames ns = go [] ns allVertexNames
  where
    go acc [] vertexNames = Right (vertexNames, reverse acc, [])
    go acc (node : rest) vertexNames
      | isNothing maybeVertex = go (node : acc) rest vertexNames
      | hasVertexPrefix vertexPrefix node
          || isNothing vertexPrefix && any isSupportVertex maybeVertex =
          isCollision node vertexNames >>= go (node : acc) rest
      | isJust maybeVertex =
          let (metaBefore, currentTree) = span isNonVertex acc
           in if null currentTree
                then Right (vertexNames, [node], reverse metaBefore ++ rest)
                else
                  Right (vertexNames, reverse currentTree, reverse metaBefore ++ (node : rest))
      | otherwise = go (node : acc) rest vertexNames
      where
        maybeVertex = newVertex node

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
addCommentToAn ic (AnnotatedVertex comments vertex meta) = AnnotatedVertex (ic : comments) vertex meta

nodesToAnnotatedVertices
  :: MetaMap
  -> [Node]
  -> Either Text (NonEmpty AnnotatedVertex)
nodesToAnnotatedVertices initialMeta nodes = go initialMeta [] nodes []
  where
    go _ _ [] acc =
      case reverse acc of
        [] -> Left "no vertices found"
        revAcc -> Right $ fromList revAcc
    go pendingMeta pendingComments (n : ns) acc =
      case newVertex n of
        Just v ->
          let av = AnnotatedVertex (reverse pendingComments) v pendingMeta
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
  -> NonEmpty Node
  -> Either Text (Set Text, VertexTreeType, VertexTree, VertexForest, [Node])
newVertexTree brks vertexNames vertexForest nodes =
  let (topNodes, nodes') = NE.span isNonVertex nodes
      topComments = mapMaybe toInternalComment topNodes
      topMeta = M.unions . map metaMapFromObject . filter isObjectNode $ topNodes
      vertexPrefix = getVertexPrefix nodes'
   in case breakVertices vertexPrefix vertexNames nodes' of
        Left err -> Left err
        Right (vertexNames', vertexNodes, rest') ->
          case nodesToAnnotatedVertices topMeta vertexNodes of
            Left err -> Left err
            Right avNE ->
              let firstAv = head avNE
                  vertexTree = VertexTree topComments (one avNE)
               in case determineGroup' brks (aVertex firstAv) of
                    Just treeType ->
                      let updatedForest = M.insertWith combineTrees treeType vertexTree vertexForest
                       in Right (vertexNames', treeType, vertexTree, updatedForest, rest')
                    Nothing -> Left "invalid breakpoint"

determineGroup :: XGroupBreakpoints -> Vertex -> Maybe VertexTreeType
determineGroup (XGroupBreakpoints brks) v =
  case [vtype | (XGroupBreakpoint f, vtype) <- brks, f (vX v)] of
    (vtype : _) -> Just vtype
    [] -> Nothing

determineGroup' :: XGroupBreakpoints -> Vertex -> Maybe VertexTreeType
determineGroup' brks v
  | isSupportVertex v = Just SupportTree
  | otherwise = determineGroup brks v

nodesListToTree
  :: XGroupBreakpoints -> NonEmpty Node -> Either Text (VertexTreeType, VertexForest)
nodesListToTree brks nodes =
  case newVertexTree brks S.empty M.empty nodes of
    Left err -> Left err
    Right (vertexNames, firstTreeType, _firstVertexTree, vertexForest, rest) ->
      case nonEmpty rest of
        Nothing -> Right (firstTreeType, vertexForest)
        Just nonEmptyRest -> go vertexNames vertexForest nonEmptyRest firstTreeType
  where
    go vertexNames acc rest firstTreeType =
      case newVertexTree brks vertexNames acc rest of
        Left err -> Left err
        Right (vertexNames', _treeType, _vt, acc', rest') ->
          case nonEmpty rest' of
            Nothing -> Right (firstTreeType, acc')
            Just ne -> go vertexNames' acc' ne firstTreeType

objectKeysToObjects :: Map Text Node -> [Node]
objectKeysToObjects =
  map (\(key, value) -> Object . V.singleton $ ObjectKey (String key, value))
    . M.assocs

extractFirstVertex
  :: NonEmpty (NonEmpty AnnotatedVertex) -> (AnnotatedVertex, [AnnotatedVertex])
extractFirstVertex (firstGroup :| otherGroups) =
  let x :| xs = firstGroup
      rest = xs ++ concatMap NE.toList otherGroups
   in (x, rest)

getVertexForestGlobals
  :: Node
  -> (VertexTreeType, VertexForest)
  -> Either Text (NonEmpty Node, VertexForest)
getVertexForestGlobals header (treeType, vertexTrees) =
  let firstVertexTree = vertexTrees M.! treeType

      (firstVertex, otherFirstTreeVertices) = extractFirstVertex (tAnnotatedVertices firstVertexTree)

      vertices =
        otherFirstTreeVertices
          ++ concatMap (NE.toList . sconcat . tAnnotatedVertices) (M.elems vertexTrees)

      isGlobal k v =
        let otherVs = map (M.lookup k . aMeta) vertices
         in all (\v' -> isNothing v' || v' == Just v) otherVs
      (globalsMap, localsMap) = M.partitionWithKey isGlobal (aMeta firstVertex)

      setLocals (AnnotatedVertex c v m) = AnnotatedVertex c v (M.union m localsMap)
      updatedForest =
        M.update
          (\(VertexTree c gs) -> Just $ VertexTree c (NE.map (NE.map setLocals) gs))
          treeType
          vertexTrees

      globalNodes = objectKeysToObjects globalsMap
   in Right (header :| globalNodes, updatedForest)

getVertexForest
  :: XGroupBreakpoints
  -> NP.NodePath
  -> Node
  -> Either Text (NonEmpty Node, VertexForest)
getVertexForest brks np topNode =
  case NP.queryNodes np topNode of
    Nothing -> Left $ "could not find vertices at path " <> show np
    Just node -> processNode node
  where
    processNode (Array ns)
      | null ns = Left "empty array at vertex path"
      | otherwise =
          case V.toList ns of
            (header : nodesWithoutHeader)
              | isValidVertexHeader header ->
                  case nonEmpty nodesWithoutHeader of
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
