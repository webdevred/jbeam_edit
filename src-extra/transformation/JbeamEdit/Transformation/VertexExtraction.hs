module JbeamEdit.Transformation.VertexExtraction (
  getVertexForest,
  determineGroup,
  determineGroup',
  metaMapFromObject,
  dropIndex,
) where

import Data.Char (isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Map.Ordered (OMap)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import JbeamEdit.Core.Node
import JbeamEdit.Core.NodePath qualified as NP
import JbeamEdit.Transformation.Config
import JbeamEdit.Transformation.OMap1 (OMap1)
import JbeamEdit.Transformation.OMap1 qualified as OMap1
import JbeamEdit.Transformation.Types

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

getVertexPrefix :: Text -> Maybe Text
getVertexPrefix firstVertexName = do
  (_, vertexIndex) <- T.unsnoc firstVertexName
  guard $ isDigit vertexIndex
  let vertexPrefix = dropIndex firstVertexName
  guard . not . T.null $ vertexPrefix
  pure vertexPrefix

getVertexPrefix' :: [Node] -> Maybe Text
getVertexPrefix' nodes = do
  firstVertexName <- getFirstVertexName nodes
  getVertexPrefix firstVertexName

isCollision :: Node -> Set Text -> Either Text (Set Text)
isCollision vertexNode vertexNames =
  case getVertexName vertexNode of
    Just vertexName ->
      if S.member vertexName vertexNames
        then Left $ "multiple vertices named " <> vertexName
        else Right (S.insert vertexName vertexNames)
    Nothing -> Right vertexNames

maybeConsComment :: [Node] -> Maybe InternalComment -> [Node]
maybeConsComment xs = maybe xs ((: xs) . Comment)

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
          let (assocPriorCmt, acc') = extractPreviousAssocCmt acc
              (metaBefore, currentTree) = span isNonVertex acc'
           in if null currentTree
                then
                  Right
                    ( vertexNames
                    , reverse (maybeConsComment [node] assocPriorCmt)
                    , metaBefore ++ rest
                    )
                else
                  Right
                    ( vertexNames
                    , reverse (maybeConsComment currentTree assocPriorCmt)
                    , metaBefore ++ (node : rest)
                    )
      | otherwise = go (node : acc) rest vertexNames
      where
        maybeVertex = newVertex node

insertTreeInForest
  :: VertexTreeType -> VertexTree -> VertexForest -> VertexForest
insertTreeInForest ttype vt f =
  if M.member ttype f
    then
      M.update (Just . insertTreeInMap vt) ttype f
    else
      M.insert
        ttype
        (OMap1.singleton (getVertexTreePrefix (tAnnotatedVertices vt), vt))
        f

getVertexTreePrefix :: NonEmpty AnnotatedVertex -> VertexTreeKey
getVertexTreePrefix vt = maybe SupportKey PrefixKey (getVertexPrefix . vName . aVertex . head $ vt)

insertTreeInMap
  :: VertexTree -> OMap1 VertexTreeKey VertexTree -> OMap1 VertexTreeKey VertexTree
insertTreeInMap (VertexTree newComments newVertexGroups) vts =
  ( getVertexTreePrefix newVertexGroups
  , VertexTree
      { tComments = newComments
      , tAnnotatedVertices = newVertexGroups
      }
  )
    `OMap1.snoc` vts

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
  -> Either Text ([Node], NonEmpty AnnotatedVertex)
nodesToAnnotatedVertices initialMeta nodes = go initialMeta [] nodes ([], [])
  where
    go _ _ [] (badNodes, acc) =
      case reverse acc of
        [] -> Left "no vertices found"
        revAcc -> Right (badNodes, fromList revAcc)
    go pendingMeta pendingComments (n : ns) acc@(badNodes, vertices) =
      case newVertex n of
        Just v ->
          let av = AnnotatedVertex (reverse pendingComments) v pendingMeta
           in go pendingMeta [] ns (badNodes, av : vertices)
        Nothing
          | isObjectNode n ->
              let newMeta = M.union (metaMapFromObject n) pendingMeta
               in go newMeta pendingComments ns acc
          | isCommentNode n ->
              case (toInternalComment n, acc) of
                (Just ic@(InternalComment _ _ PreviousNode), (_, an : ans)) ->
                  go pendingMeta pendingComments ns (badNodes, addCommentToAn ic an : ans)
                (Just ic, _) ->
                  go pendingMeta (ic : pendingComments) ns acc
                (Nothing, _) ->
                  go pendingMeta pendingComments ns acc
          | otherwise -> go pendingMeta pendingComments ns (n : badNodes, vertices)

newVertexTree
  :: XGroupBreakpoints
  -> Set Text
  -> [Node]
  -> VertexForest
  -> NonEmpty Node
  -> Either Text (Set Text, [Node], VertexTreeType, VertexTree, VertexForest, [Node])
newVertexTree brks vertexNames badAcc vertexForest nodes =
  let (topNodes, nodes') = NE.span isNonVertex nodes
      topComments = mapMaybe toInternalComment topNodes
      topMeta = M.unions . map metaMapFromObject $ topNodes
      vertexPrefix = getVertexPrefix' nodes'
   in case breakVertices vertexPrefix vertexNames nodes' of
        Left err -> Left err
        Right (vertexNames', vertexNodes, rest') ->
          case nodesToAnnotatedVertices topMeta vertexNodes of
            Left err -> Left err
            Right (badNodes, avNE) ->
              let firstAV = head avNE
                  vertexTree = VertexTree topComments avNE
               in case determineGroup brks (aVertex firstAV) of
                    Just treeType ->
                      let updatedForest = insertTreeInForest treeType vertexTree vertexForest
                       in Right
                            (vertexNames', badAcc <> badNodes, treeType, vertexTree, updatedForest, rest')
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
  -> NonEmpty Node
  -> Either Text ([Node], VertexTreeType, VertexForest)
nodesListToTree brks nodes =
  case newVertexTree brks S.empty [] M.empty nodes of
    Left err -> Left err
    Right
      (vertexNames, badNodes, firstTreeType, _firstVertexTree, vertexForest, rest) ->
        case nonEmpty rest of
          Nothing -> Right (badNodes, firstTreeType, vertexForest)
          Just nonEmptyRest -> go vertexNames badNodes vertexForest nonEmptyRest firstTreeType
  where
    go vertexNames badNodes acc rest firstTreeType =
      case newVertexTree brks vertexNames badNodes acc rest of
        Left err -> Left err
        Right (vertexNames', badNodes', _treeType, _vt, acc', rest') ->
          case nonEmpty rest' of
            Nothing -> Right (badNodes, firstTreeType, acc')
            Just ne -> go vertexNames' (badNodes ++ badNodes') acc' ne firstTreeType

objectKeysToObjects :: Map Text Node -> [Node]
objectKeysToObjects =
  map (\(key, value) -> Object . V.singleton $ ObjectKey (String key, value))
    . M.assocs

concatAnnotatedVertices
  :: OMap VertexTreeKey VertexTree
  -> [AnnotatedVertex]
concatAnnotatedVertices = concatMap (toList . tAnnotatedVertices) . toList

extractFirstVertex
  :: OMap1 VertexTreeKey VertexTree -> (AnnotatedVertex, [AnnotatedVertex])
extractFirstVertex vertexTrees =
  let (_, firstTree, otherTrees) = OMap1.uncons vertexTrees
      (VertexTree _ (firstAV :| otherFirstAVs)) = firstTree
      rest =
        otherFirstAVs ++ concatAnnotatedVertices otherTrees
   in (firstAV, rest)

allAnnotatedVertices :: VertexForest -> [AnnotatedVertex]
allAnnotatedVertices forest =
  [ av
  | omap <- M.elems forest
  , tree <- toList omap
  , av <- NE.toList (tAnnotatedVertices tree)
  ]

getVertexForestGlobals
  :: [Node]
  -> Node
  -> (VertexTreeType, VertexForest)
  -> Either Text ([Node], NonEmpty Node, VertexForest)
getVertexForestGlobals badNodes header (treeType, vertexTrees) =
  let firstVertexTree = vertexTrees M.! treeType

      (firstVertex, otherFirstTreeVertices) = extractFirstVertex firstVertexTree

      vertices =
        otherFirstTreeVertices ++ allAnnotatedVertices vertexTrees

      isGlobal k v =
        let otherVs = map (M.lookup k . aMeta) vertices
         in all (\v' -> isNothing v' || v' == Just v) otherVs
      (globalsMap, localsMap) = M.partitionWithKey isGlobal (aMeta firstVertex)

      setLocals (AnnotatedVertex c v m) = AnnotatedVertex c v (M.union m localsMap)
      updatedForest =
        M.update
          (Just . fmap (\(VertexTree c gs) -> VertexTree c (NE.map setLocals gs)))
          treeType
          vertexTrees

      globalNodes = objectKeysToObjects globalsMap
   in Right (badNodes, header :| globalNodes, updatedForest)

getVertexForest
  :: XGroupBreakpoints
  -> NP.NodePath
  -> Node
  -> Either Text ([Node], NonEmpty Node, VertexForest)
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
                        Right (badNodes, firstTreeType, vertexForest) ->
                          getVertexForestGlobals badNodes header (firstTreeType, vertexForest)
              | otherwise -> Left "invalid vertex header"
            _ -> Left "missing vertex header"
    processNode bad = Left $ "expected Array at vertex path, got: " <> show bad

isValidVertexHeader :: Node -> Bool
isValidVertexHeader (Array header) =
  V.length header == 4 && all isStringNode header
isValidVertexHeader _ = False
