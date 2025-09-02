module VertexExtraction (getVertexForest, determineGroup, isSupportVertex, dropIndex) where

import Core.Node
import Data.Char (isDigit)
import Data.List (partition)
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
  :: Maybe Text -> Set Text -> [Node] -> Either Text (Set Text, [Node], [Node])
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

isSupportVertex :: Vertex -> Bool
isSupportVertex v =
  case T.unsnoc (vName v) of
    Nothing -> True
    Just (_, c) -> not (isDigit c)

nodesListToTree
  :: NonEmpty Node -> Either Text (VertexTreeType, VertexTree, VertexForest)
nodesListToTree nodes =
  case newVertexTree S.empty M.empty nodes of
    Right (vertexNames, firstTreeType, firstVertexTree, vertexForest, rest) ->
      case nonEmpty rest of
        Just nonEmptyRest ->
          case go vertexNames vertexForest nonEmptyRest of
            Right finalForest -> Right (firstTreeType, firstVertexTree, finalForest)
            Left err -> Left err
        Nothing -> Right (firstTreeType, firstVertexTree, vertexForest)
    Left err -> Left err
  where
    go :: Set Text -> VertexForest -> NonEmpty Node -> Either Text VertexForest
    go vertexNames acc restNE =
      case newVertexTree vertexNames acc restNE of
        Right (vertexNames', _treeType, _, acc', rest') ->
          case nonEmpty rest' of
            Nothing -> Right acc'
            Just ne -> go vertexNames' acc' ne
        Left err -> Left err

createVertexForFirstNode :: [Node] -> Maybe (Vertex, [Node])
createVertexForFirstNode nodes = do
  (node, rest) <- uncons nodes
  vertex <- newVertex node
  pure (vertex, rest)

toVertexTreeEntry :: Node -> Either Text VertexTreeEntry
toVertexTreeEntry node =
  case newVertex node of
    Just vertice -> Right $ VertexEntry vertice
    Nothing ->
      case node of
        (Object meta) -> Right $ MetaEntry meta
        (Comment comment) -> Right $ CommentEntry comment
        _ -> Left $ "unexpected node " <> show node

newVertexTree
  :: Set Text
  -> VertexForest
  -> NonEmpty Node
  -> Either Text (Set Text, VertexTreeType, VertexTree, VertexForest, [Node])
newVertexTree vertexNames vertexForest nodes' =
  let (nonVertices, rest) = NE.span isNonVertex nodes'
      vertexPrefix = getVertexPrefix rest
   in case breakVertices vertexPrefix vertexNames rest of
        Right (vertexNames', vertexNodes, rest') ->
          case createVertexForFirstNode vertexNodes of
            Nothing -> Left "no vertices found when building a vertex tree"
            Just (firstV, vs) ->
              case mapM toVertexTreeEntry vs of
                Right vertexEntries ->
                  let treeType = determineGroup firstV
                      vertexTree = VertexTree nonVertices (VertexEntry firstV :| vertexEntries)
                      updatedForest =
                        case treeType of
                          SupportTree ->
                            M.insertWith combineSupportTrees SupportTree vertexTree vertexForest
                          _ -> M.insert treeType vertexTree vertexForest
                   in Right (vertexNames', treeType, vertexTree, updatedForest, rest')
                Left err -> Left err
        Left err -> Left err

combineSupportTrees :: VertexTree -> VertexTree -> VertexTree
combineSupportTrees (VertexTree metas1 entries1) (VertexTree metas2 entries2) =
  VertexTree
    { tMetaNodes = metas1 ++ metas2
    , tVertexNodes = entries1 <> entries2
    }

getVertexForest
  :: NP.NodePath -> Node -> Either Text (NonEmpty Node, VertexForest)
getVertexForest np topNode =
  case NP.queryNodes np topNode of
    Nothing -> Left $ "could not find vertices at path " <> show np
    Just node -> processNode node
  where
    processNode (Array ns)
      | null ns = Left "empty array at vertex path"
      | otherwise =
          case nodesListToTree (NE.fromList (V.toList ns)) of
            Left err -> Left err
            Right (firstTreeType, firstVertexTree, vertexForest) ->
              getVertexForestGlobals (firstTreeType, firstVertexTree, vertexForest)
    processNode bad = Left $ "expected Array at vertex path, got: " <> show bad

determineGroup :: Vertex -> VertexTreeType
determineGroup v
  | isSupportVertex v = SupportTree
  | vX v > 0.09 = LeftTree
  | vX v > -0.09 = MiddleTree
  | otherwise = RightTree

objectsToObjectKeys :: Node -> Maybe Object
objectsToObjectKeys (Object keys) = Just keys
objectsToObjectKeys _ = Nothing

getVertexForestGlobals
  :: (VertexTreeType, VertexTree, VertexForest)
  -> Either Text (NonEmpty Node, VertexForest)
getVertexForestGlobals (treeType, firstVertexTree, vertexTrees) =
  let metaElem globalMeta localMeta =
        let maybeKey = metaKey globalMeta
         in case maybeKey of
              Nothing -> True
              Just meta -> meta `S.member` localMeta
      extractLocalMeta = S.unions . map metaKeys . NE.toList . tVertexNodes
      extractMeta' = S.fromList . mapMaybe metaKey . tMetaNodes
      restMeta =
        let localMeta = extractLocalMeta firstVertexTree
            topMetaOtherTrees = foldMap extractMeta' (M.delete treeType vertexTrees)
            localMetaOtherTrees = foldMap extractLocalMeta vertexTrees
         in S.unions [localMeta, topMetaOtherTrees, localMetaOtherTrees]
      existsInTree globalMeta = globalMeta `metaElem` restMeta
   in case firstVertexTree of
        VertexTree (header : metasWithoutHeader) cg
          | isValidVertexHeader header ->
              let objectKeysToObjects = V.toList . V.map (Object . V.singleton)
                  (topComments, topMetas) = partition isCommentNode metasWithoutHeader
                  ungroupedMetaKeys = V.concat (mapMaybe objectsToObjectKeys topMetas)
                  (localMetas, globalMetas) = V.partition existsInTree ungroupedMetaKeys
               in Right
                    ( header :| objectKeysToObjects globalMetas
                    , M.insert
                        treeType
                        (VertexTree (topComments ++ objectKeysToObjects localMetas) cg)
                        vertexTrees
                    )
          | otherwise -> Left "invalid vertex header"
        _ -> Left "missing vertex header"

metaKey :: Node -> Maybe Text
metaKey (ObjectKey (String key, _)) = Just key
metaKey _ = Nothing

isValidVertexHeader :: Node -> Bool
isValidVertexHeader (Array header) =
  V.length header == 4 && all isStringNode header
isValidVertexHeader _ = False

metaKeys :: VertexTreeEntry -> Set Text
metaKeys (MetaEntry m) = S.fromList . mapMaybe metaKey . V.toList $ m
metaKeys _ = S.empty
