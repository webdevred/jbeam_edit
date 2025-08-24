{-# LANGUAGE RankNTypes #-}

module Transformation (
  transform,
) where

import Control.Monad (foldM, guard)
import Core.Node
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (partition, sortOn, uncons)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe)
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector, (!), (!?), (//))
import GHC.IsList (fromList)

import Core.NodeCursor qualified as NC
import Core.NodePath qualified as NP
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable qualified as TR (mapAccumL)
import Data.Vector qualified as V

type VertexForest = Map VertexTreeType VertexTree

data VertexTreeType
  = LeftTree
  | MiddleTree
  | RightTree
  | SupportTree
  deriving (Eq, Ord, Show)

data VertexTreeEntry
  = VertexEntry Vertex
  | CommentEntry InternalComment
  | MetaEntry Object
  deriving (Eq, Show)

data VertexTree = VertexTree
  { tMetaNodes :: [Node]
  , tVertexNodes :: NonEmpty VertexTreeEntry
  }
  deriving (Show)

data Vertex = Vertex
  { vName :: Text
  , vX :: Scientific
  , vY :: Scientific
  , vZ :: Scientific
  , vMeta :: Maybe Object
  }
  deriving (Eq, Show)

data CommentGroup = CommentGroup
  { cComments :: [InternalComment]
  , cVertex :: Vertex
  , cMeta :: MetaMap
  }
  deriving (Show)

type MetaMap = Map Text Node

showAsText :: forall a. Show a => a -> Text
showAsText = T.pack . show

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
hasVertexPrefix vertexPrefix node =
  let vertexName = dropIndex <$> getVertexName node
   in vertexName == (dropIndex <$> vertexPrefix)

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
  let vertexPrefix = T.dropWhileEnd isDigit firstVertexName
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
      case NE.nonEmpty rest of
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
          case NE.nonEmpty rest' of
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
        _ -> Left $ "unexpected node " <> showAsText node

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
  :: NP.NodePath -> Node -> Either Text (VertexTreeType, VertexTree, VertexForest)
getVertexForest np topNode =
  case NP.queryNodes np topNode of
    Just node -> f node
    Nothing -> Left $ "could not find vertices at path " <> showAsText verticesQuery
  where
    f node =
      case node of
        Array ns
          | null ns -> Left . showAsText $ ns
          | otherwise -> nodesListToTree . NE.fromList . V.toList $ ns
        bad -> Left . showAsText $ bad

determineGroup :: Vertex -> VertexTreeType
determineGroup v
  | isSupportVertex v = SupportTree
  | vX v > 0.09 = LeftTree
  | vX v > -0.09 = MiddleTree
  | otherwise = RightTree

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

updateVertexNames :: Int -> VertexTreeEntry -> (Int, VertexTreeEntry)
updateVertexNames index (VertexEntry vertex) =
  let vertexName = vName vertex
   in if T.all (not . isDigit) vertexName
        then (index, VertexEntry vertex)
        else
          let newIndex = index + 1
              vertexPrefix = dropIndex vertexName
              newVertexName = vertexPrefix <> T.pack (show index)
              renamedVertex = VertexEntry (vertex {vName = newVertexName})
           in (newIndex, renamedVertex)
updateVertexNames index entry = (index, entry)

nodesToMap :: [Node] -> MetaMap
nodesToMap nodes = M.fromList $ concatMap extractMeta nodes
  where
    extractMeta :: Node -> [(Text, Node)]
    extractMeta (Object obj) =
      [ (kText, v)
      | ObjectKey (String kText, v) <- V.toList obj
      ]
    extractMeta _ = []

groupVertexWithLeadingCommentsAndMeta :: VertexTree -> [CommentGroup]
groupVertexWithLeadingCommentsAndMeta (VertexTree baseMeta entriesNE) =
  go (nodesToMap baseMeta) [] (NE.toList entriesNE)
  where
    go _ _ [] = []
    go currentMeta accComments (entry : rest) =
      case entry of
        MetaEntry obj ->
          let newMeta = M.union (nodesToMap [Object obj]) currentMeta
           in go newMeta accComments rest
        CommentEntry comment ->
          go currentMeta (accComments ++ [comment]) rest
        VertexEntry v ->
          let cg = CommentGroup {cComments = accComments, cVertex = v, cMeta = currentMeta}
           in cg : go currentMeta [] rest

renameVertexId :: VertexTreeType -> Text -> Text
renameVertexId treeType vertexName =
  let prefix = T.takeWhile (not . isDigit) vertexName
      index = T.dropWhile (not . isDigit) vertexName
      newPrefix = case treeType of
        LeftTree -> if "l" `T.isSuffixOf` prefix then prefix else prefix <> "l"
        MiddleTree -> if "m" `T.isSuffixOf` prefix then prefix else prefix <> "m"
        RightTree -> if "r" `T.isSuffixOf` prefix then prefix else prefix <> "r"
        SupportTree -> prefix
   in newPrefix <> index

sideCommentText :: VertexTreeType -> Text
sideCommentText LeftTree = "Left side"
sideCommentText MiddleTree = "Middle side"
sideCommentText RightTree = "Right side"
sideCommentText SupportTree = "Support"

sideComment :: VertexTreeType -> InternalComment
sideComment t = InternalComment (sideCommentText t) False

metaKeysFromNodes :: [Node] -> Set Text
metaKeysFromNodes nodes = S.fromList . M.keys $ nodesToMap nodes

buildTree
  :: VertexForest -> VertexTreeType -> [CommentGroup] -> Either Text VertexTree
buildTree vertexTrees t groupsOrig =
  let groupsSorted = sortCommentGroups groupsOrig
      origTree = M.lookup t vertexTrees
      topMetas = maybe [] tMetaNodes origTree

      existingTopKeys = metaKeysFromNodes topMetas
      existingEntryKeys = maybe S.empty (S.unions . map metaKeys . NE.toList . tVertexNodes) origTree
      initialKeys = existingTopKeys <> existingEntryKeys

      existingNames :: Set Text
      existingNames =
        maybe
          S.empty
          (S.fromList . mapMaybe (fmap vName . possiblyVertex) . NE.toList . tVertexNodes)
          origTree

      sideCommentEntry :: [VertexTreeEntry]
      sideCommentEntry =
        ([CommentEntry (sideComment t) | isNothing origTree && not (null groupsSorted)])

      process [] keys = ([], keys)
      process (cg : rest) keys =
        let cm = cMeta cg
            needed = [k | (k, _) <- M.assocs cm]
            missing = filter (`S.notMember` keys) needed
            newMetaEntries = [MetaEntry (V.singleton (ObjectKey (String k, cm M.! k))) | k <- missing]
            commentEntries = map CommentEntry (cComments cg)

            originalVertex = cVertex cg
            finalName =
              if S.member (vName originalVertex) existingNames
                then vName originalVertex
                else renameVertexId t (vName originalVertex)

            vertexEntry = VertexEntry (originalVertex {vName = finalName})
            newKeys = keys <> S.fromList missing
            (bundlesRest, finalKeys) = process rest newKeys
         in (newMetaEntries ++ commentEntries ++ (vertexEntry : bundlesRest), finalKeys)

      (bundles, _) = process groupsSorted initialKeys

      finalEntries = sideCommentEntry ++ bundles
   in case NE.nonEmpty finalEntries of
        Just ne -> Right $ VertexTree topMetas ne
        Nothing -> Left "Cannot build empty tree: there are no vertices to insert"

moveVerticesInVertexForest :: VertexForest -> Either Text VertexForest
moveVerticesInVertexForest vertexTrees = do
  let supportTree = M.lookup SupportTree vertexTrees
      movableTrees = M.delete SupportTree vertexTrees

      allGroups :: [CommentGroup]
      allGroups = concatMap groupVertexWithLeadingCommentsAndMeta (M.elems movableTrees)

      movableGroups :: [CommentGroup]
      movableGroups = filter (not . isSupportVertex . cVertex) allGroups

      grouped :: Map VertexTreeType [CommentGroup]
      grouped = M.fromListWith (++) [(determineGroup (cVertex g), [g]) | g <- movableGroups]

  newForest <-
    foldM
      ( \acc k ->
          case M.lookup k grouped of
            Just groupsForK | not (null groupsForK) -> do
              tree <- buildTree vertexTrees k groupsForK
              pure (M.insert k tree acc)
            _ -> pure $ maybe acc (\ot -> M.insert k ot acc) (M.lookup k vertexTrees)
      )
      M.empty
      [LeftTree, MiddleTree, RightTree]

  let finalForest = maybe newForest (\st -> M.insert SupportTree st newForest) supportTree
  pure finalForest

updateVertexForest :: NonEmpty Node -> VertexForest -> Either Text VertexForest
updateVertexForest globalMetas vertexForest =
  let updated = moveVerticesInVertexForest vertexForest
      addGlobalMetas f =
        case listToMaybe $ M.toList f of
          Nothing -> f
          Just (firstTreeType, firstVertexTree) ->
            let VertexTree metas entries = firstVertexTree
                firstVertexTreeWithGlobals = VertexTree (NE.toList globalMetas ++ metas) entries
             in M.insert firstTreeType firstVertexTreeWithGlobals f
   in sortVertices . addGlobalMetas <$> updated

groupByMeta :: [VertexTreeEntry] -> [[VertexTreeEntry]]
groupByMeta [] = []
groupByMeta (x : xs)
  | isMeta x =
      let (grp, rest) = break isMeta xs
       in (x : grp) : groupByMeta rest
  | otherwise =
      case groupByMeta xs of
        [] -> [[x]]
        (g : gs) -> (x : g) : gs

isMeta :: VertexTreeEntry -> Bool
isMeta (VertexEntry _) = False
isMeta _ = True

groupVertexWithLeadingComments :: [VertexTreeEntry] -> [CommentGroup]
groupVertexWithLeadingComments = go [] []
  where
    go _ _ [] = []
    go metas acc (e : rest) =
      case e of
        MetaEntry obj -> go [Object obj] acc rest
        CommentEntry c -> go metas (acc ++ [c]) rest
        VertexEntry v ->
          let cg = CommentGroup {cComments = acc, cVertex = v, cMeta = nodesToMap metas}
           in cg : go [] [] rest

sortCommentGroups :: [CommentGroup] -> [CommentGroup]
sortCommentGroups = sortOn (\cg -> (vZ (cVertex cg), vY (cVertex cg)))

ungroupCommentGroups :: NonEmpty CommentGroup -> VertexTree
ungroupCommentGroups ((CommentGroup comments vertex meta) :| commentGroups) =
  let allMetas :: MetaMap
      allMetas = M.unions (meta : map cMeta commentGroups)

      headerNodes :: [Node]
      headerNodes =
        [Object (V.singleton (ObjectKey (String k, v))) | (k, v) <- M.assocs allMetas]

      entries :: NonEmpty VertexTreeEntry
      entries =
        map CommentEntry comments
          `NE.prependList` (VertexEntry vertex :| concatMap toEntries commentGroups)

      toEntries cg = map CommentEntry (cComments cg) ++ [VertexEntry (cVertex cg)]
   in VertexTree headerNodes entries

processGroup :: [VertexTreeEntry] -> [VertexTreeEntry]
processGroup [] = []
processGroup (metaOrHeader : rest) =
  let commentGroups = groupVertexWithLeadingComments rest
      sortedGroups = sortCommentGroups commentGroups
   in case NE.nonEmpty sortedGroups of
        Nothing -> [metaOrHeader]
        Just ne ->
          let entriesSorted = ungroupCommentGroups ne
           in metaOrHeader : NE.toList (tVertexNodes entriesSorted)

sortVertices :: VertexForest -> VertexForest
sortVertices = M.map sortVerticesInForest
  where
    sortVerticesInForest (VertexTree metas nodes) =
      let groups = groupByMeta (NE.toList nodes)
          processedGroups = concatMap processGroup groups
          (_, nodes') = TR.mapAccumL updateVertexNames 0 (NE.fromList processedGroups)
       in VertexTree metas nodes'

verticesQuery :: NP.NodePath
verticesQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "nodes"]

possiblyVertex :: VertexTreeEntry -> Maybe Vertex
possiblyVertex (VertexEntry v) = Just v
possiblyVertex _ = Nothing

getVertexNamesInForest
  :: VertexForest -> M.Map (Scientific, Scientific, Scientific) Text
getVertexNamesInForest vertexTrees = M.unions $ M.map getVertexNamesInTree vertexTrees
  where
    getVertexNamesInTree (VertexTree _ entries) =
      let vertexCordNamePair vertex =
            ((vX vertex, vY vertex, vZ vertex), vName vertex)
          getVertexNames =
            M.fromList
              . mapMaybe (fmap vertexCordNamePair . possiblyVertex)
              . NE.toList
       in getVertexNames entries

isObjectKeyEqual :: NP.NodeSelector -> Node -> Bool
isObjectKeyEqual (NP.ObjectKey a) (ObjectKey (String b, _)) = a == b
isObjectKeyEqual _ _ = False

vertexForestToNodeVector :: VertexForest -> Vector Node
vertexForestToNodeVector = V.concat . map vertexTreeToNodeVector . M.elems
  where
    vertexTreeToNodeVector (VertexTree metas vertices) =
      let currentNodes = V.fromList $ metas ++ (NE.toList . NE.map vertexEntryToNode $ vertices)
          vertexEntryToNode entry =
            case entry of
              (CommentEntry node) -> Comment node
              (MetaEntry node) -> Object node
              (VertexEntry vertex) ->
                let name = String . vName $ vertex
                    x = Number . vX $ vertex
                    y = Number . vY $ vertex
                    z = Number . vZ $ vertex
                    possiblyMeta = maybe [] (pure . Object) (vMeta vertex)
                 in Array . fromList $ [name, x, y, z] ++ possiblyMeta
       in currentNodes

updateVerticesInNode :: NP.NodePath -> VertexForest -> Node -> Node
updateVerticesInNode (NP.NodePath Empty) g (Array _) =
  Array (vertexForestToNodeVector g)
updateVerticesInNode (NP.NodePath ((NP.ArrayIndex i) :<| qrest)) g (Array children) =
  let updateInNode nodeToUpdate =
        children
          // [(i, updateVerticesInNode (NP.NodePath qrest) g nodeToUpdate)]
   in Array $ maybe children updateInNode (children !? i)
updateVerticesInNode (NP.NodePath ((NP.ObjectIndex i) :<| qrest)) g (Object children) =
  let updateInNode _ =
        children
          // [(i, updateVerticesInNode (NP.NodePath qrest) g (children ! i))]
   in Object $ maybe children updateInNode (children !? i)
updateVerticesInNode (NP.NodePath (k@(NP.ObjectKey _) :<| qrest)) g (Object children) =
  let updateInNode i =
        children
          // [(i, updateVerticesInNode (NP.NodePath qrest) g (children ! i))]
   in Object . maybe children updateInNode $
        V.findIndex (isObjectKeyEqual k) children
updateVerticesInNode query g (ObjectKey (k, v)) =
  ObjectKey (k, updateVerticesInNode query g v)
updateVerticesInNode _ _ a = a

findAndUpdateTextInNode :: Map Text Text -> NC.NodeCursor -> Node -> Node
findAndUpdateTextInNode m cursor node =
  case node of
    Array arr
      | NC.comparePathAndCursor verticesQuery cursor -> Array arr
      | otherwise -> Array $ V.imap applyBreadcrumbAndUpdateText arr
    Object obj -> Object $ V.imap applyBreadcrumbAndUpdateText obj
    ObjectKey (key, value) ->
      ObjectKey
        (key, NC.applyObjCrumb key cursor (findAndUpdateTextInNode m) value)
    String s -> String $ M.findWithDefault s s m
    _ -> node
  where
    applyBreadcrumbAndUpdateText index =
      NC.applyCrumb (NC.ArrayIndex index) cursor (findAndUpdateTextInNode m)

getVertexForestGlobals
  :: (VertexTreeType, VertexTree, VertexForest)
  -> Either Text (NonEmpty Node, VertexForest)
getVertexForestGlobals (treeType, firstVertexTree, vertexTrees) =
  let metaElem globalMeta localMeta =
        let maybeKey = metaKey globalMeta
         in case maybeKey of
              Nothing -> False
              Just meta -> meta `S.member` localMeta
      extractLocalMeta = S.unions . map metaKeys . NE.toList . tVertexNodes
      extractMeta' = S.fromList . mapMaybe metaKey . tMetaNodes
      restMeta =
        let localMeta = extractLocalMeta firstVertexTree
            topMetaOtherTrees = foldMap extractMeta' vertexTrees
            localMetaOtherTrees = foldMap extractLocalMeta vertexTrees
         in localMeta <> topMetaOtherTrees <> localMetaOtherTrees
      existsInTree globalMeta = globalMeta `metaElem` restMeta
   in case firstVertexTree of
        VertexTree (header : metasWithoutHeader) cg
          | isValidVertexHeader header ->
              let (localMetas, globalMetas) = partition existsInTree metasWithoutHeader
               in Right
                    ( header :| globalMetas
                    , M.insert treeType (VertexTree localMetas cg) vertexTrees
                    )
          | otherwise -> Left "invalid vertex header"
        _ -> Left "missing vertex header"

transform :: NC.NodeCursor -> Node -> Either Text Node
transform cursor topNode =
  getVertexForest verticesQuery topNode
    >>= getVertexForestGlobals
    >>= getNamesAndUpdateTree
  where
    getNamesAndUpdateTree (globals, vertexForest) =
      let vertexNames = getVertexNamesInForest vertexForest
       in updateVertexForest globals vertexForest
            >>= getUpdatedNamesAndUpdateGlobally vertexNames
    getUpdatedNamesAndUpdateGlobally oldVertexNames updatedVertexForest =
      let updatedVertexNames = getVertexNamesInForest updatedVertexForest
          updateMap = M.fromList $ on zip M.elems oldVertexNames updatedVertexNames
       in Right . findAndUpdateTextInNode updateMap cursor $
            updateVerticesInNode verticesQuery updatedVertexForest topNode
