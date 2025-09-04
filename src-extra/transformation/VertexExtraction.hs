module VertexExtraction (
  getVertexForest,
  determineGroup,
  isSupportVertex,
  metaMapFromObject,
  dropIndex,
) where

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
combineTrees (VertexTree _ groups1) (VertexTree comments2 groups2) =
  VertexTree
    { tComments = comments2
    , tCommentGroups = groups1 <> groups2
    }

isSupportVertex :: Vertex -> Bool
isSupportVertex v =
  case T.unsnoc (vName v) of
    Nothing -> True
    Just (_, c) -> not (isDigit c)

metaMapFromObject :: Node -> Map Text Node
metaMapFromObject (Object objKeys) =
  let toKV (ObjectKey (String k, v)) = Just (k, v)
      toKV _ = Nothing
   in M.fromList . mapMaybe toKV $ V.toList objKeys
metaMapFromObject _ = M.empty

toInternalComment :: Node -> Maybe InternalComment
toInternalComment (Comment c) = Just c
toInternalComment _ = Nothing

nodesToCommentGroups
  :: Map Text Node
  -> [Node]
  -> Either Text (NE.NonEmpty CommentGroup)
nodesToCommentGroups initialMeta nodes = go initialMeta [] nodes []
  where
    go _ _ [] acc =
      case reverse acc of
        [] -> Left "no vertices found"
        revAcc -> Right $ NE.fromList revAcc
    go pendingMeta pendingComments (n : ns) acc =
      case newVertex n of
        Just v ->
          let cg = CommentGroup (reverse pendingComments) v pendingMeta
           in go pendingMeta [] ns (cg : acc)
        Nothing
          | isObjectNode n ->
              let newMeta = M.union (metaMapFromObject n) pendingMeta
               in go newMeta pendingComments ns acc
          | isCommentNode n ->
              case toInternalComment n of
                Just ic -> go pendingMeta (ic : pendingComments) ns acc
                Nothing -> go pendingMeta pendingComments ns acc
          | otherwise -> go pendingMeta pendingComments ns acc

newVertexTree
  :: Set Text
  -> VertexForest
  -> NE.NonEmpty Node
  -> Either Text (Set Text, VertexTreeType, VertexTree, VertexForest, [Node])
newVertexTree vertexNames vertexForest nodes =
  let (topNodes, nodes') = NE.span isNonVertex nodes
      topComments = mapMaybe toInternalComment topNodes
      topMeta = M.unions . map metaMapFromObject . filter isObjectNode $ topNodes
      vertexPrefix = getVertexPrefix nodes'
   in case breakVertices vertexPrefix vertexNames nodes' of
        Left err -> Left err
        Right (vertexNames', vertexNodes, rest') ->
          case nodesToCommentGroups topMeta vertexNodes of
            Left err -> Left err
            Right cgNe ->
              let firstCG = NE.head cgNe
                  vertexTree = VertexTree topComments cgNe
                  treeType = determineGroup (cVertex firstCG)
                  updatedForest = M.insertWith combineTrees treeType vertexTree vertexForest
               in Right (vertexNames', treeType, vertexTree, updatedForest, rest')

determineGroup :: Vertex -> VertexTreeType
determineGroup v
  | isSupportVertex v = SupportTree
  | vX v > 0.09 = LeftTree
  | vX v > -0.09 = MiddleTree
  | otherwise = RightTree

nodesListToTree
  :: NE.NonEmpty Node -> Either Text (VertexTreeType, VertexTree, VertexForest)
nodesListToTree nodes =
  case newVertexTree S.empty M.empty nodes of
    Left err -> Left err
    Right (vertexNames, firstTreeType, firstVertexTree, vertexForest, rest) ->
      case nonEmpty rest of
        Nothing -> Right (firstTreeType, firstVertexTree, vertexForest)
        Just nonEmptyRest -> go vertexNames vertexForest nonEmptyRest firstTreeType firstVertexTree
  where
    go vertexNames acc restNE firstTreeType firstVertexTree =
      case newVertexTree vertexNames acc restNE of
        Left err -> Left err
        Right (vertexNames', _treeType, _vt, acc', rest') ->
          case nonEmpty rest' of
            Nothing -> Right (firstTreeType, firstVertexTree, acc')
            Just ne -> go vertexNames' acc' ne firstTreeType firstVertexTree

objectKeysToObjects :: Map Text Node -> [Node]
objectKeysToObjects =
  map (\(key, value) -> Object . V.singleton $ ObjectKey (String key, value))
    . M.assocs

getVertexForestGlobals
  :: Node
  -> (VertexTreeType, VertexTree, VertexForest)
  -> Either Text (NE.NonEmpty Node, VertexForest)
getVertexForestGlobals header (treeType, firstVertexTree, vertexTrees) =
  let allFirstCGs = tCommentGroups firstVertexTree
      (firstCG, laterFirstCGsMaybe) = NE.uncons allFirstCGs
      laterFirstCGsList = maybe [] NE.toList laterFirstCGsMaybe
      allOtherTrees = M.delete treeType vertexTrees
      treesNoSupport = M.delete SupportTree allOtherTrees
      allOtherCGs = concatMap (NE.toList . tCommentGroups) (M.elems treesNoSupport)

      isGlobal :: Text -> Node -> Bool
      isGlobal k v =
        all
          ( \cg -> case M.lookup k (cMeta cg) of
              Nothing -> True
              Just v' -> v' == v
          )
          (laterFirstCGsList ++ allOtherCGs)

      (globalsMap, localsMap) = M.partitionWithKey isGlobal (cMeta firstCG)
      setLocals (CommentGroup c v m) = CommentGroup c v (M.union m localsMap)
      updatedForest =
        M.update
          (\(VertexTree c gs) -> Just $ VertexTree c (NE.map setLocals gs))
          treeType
          vertexTrees
      globalNodes = objectKeysToObjects globalsMap
   in Right (header :| globalNodes, updatedForest)

getVertexForest
  :: NP.NodePath -> Node -> Either Text (NE.NonEmpty Node, VertexForest)
getVertexForest np topNode =
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
                    Just ne -> do
                      case nodesListToTree ne of
                        Left err -> Left err
                        Right (firstTreeType, firstVertexTree, vertexForest) ->
                          getVertexForestGlobals header (firstTreeType, firstVertexTree, vertexForest)
              | otherwise -> Left "invalid vertex header"
            _ -> Left "missing vertex header"
    processNode bad = Left $ "expected Array at vertex path, got: " <> show bad

isValidVertexHeader :: Node -> Bool
isValidVertexHeader (Array header) =
  V.length header == 4 && all isStringNode header
isValidVertexHeader _ = False
