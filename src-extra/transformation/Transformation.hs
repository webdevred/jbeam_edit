module Transformation (
  transform,
) where

import Control.Monad (foldM)
import Core.Node
import Core.NodeCursor (newCursor)
import Data.Char (isDigit)
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Vector (Vector, (!), (!?), (//))
import Types
import VertexExtraction

import Core.NodeCursor qualified as NC
import Core.NodePath qualified as NP
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V

updateVertexNames :: Int -> VertexTreeEntry -> (Int, VertexTreeEntry)
updateVertexNames index (VertexEntry vertex) =
  let vertexName = vName vertex
   in if T.all (not . isDigit) vertexName
        then (index, VertexEntry vertex)
        else
          let newIndex = index + 1
              vertexPrefix = dropIndex vertexName
              newVertexName = vertexPrefix <> show index
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

existingNames :: Maybe VertexTree -> Set Text
existingNames =
  maybe
    S.empty
    (S.fromList . mapMaybe (fmap vName . possiblyVertex) . NE.toList . tVertexNodes)

process
  :: Set Text -> VertexTreeType -> MetaMap -> [CommentGroup] -> [VertexTreeEntry]
process _ _ _ [] = []
process oldNames t prevMeta (cg : rest) =
  let changedMeta = M.differenceWith diff (cMeta cg) prevMeta
      diff new old = if new == old then Nothing else Just new

      metaEntries =
        [ MetaEntry (V.singleton (ObjectKey (String k, v)))
        | (k, v) <- M.assocs changedMeta
        ]

      commentEntries = map CommentEntry (cComments cg)

      originalVertex = cVertex cg
      finalName =
        if S.member (vName originalVertex) oldNames
          then vName originalVertex
          else renameVertexId t (vName originalVertex)
      vertexEntry = VertexEntry (originalVertex {vName = finalName})

      entries = metaEntries ++ commentEntries ++ [vertexEntry]
   in entries ++ process oldNames t (cMeta cg) rest

buildTree
  :: VertexForest -> VertexTreeType -> [CommentGroup] -> Either Text VertexTree
buildTree vertexTrees t groupsOrig =
  let groupsSorted = sortCommentGroups groupsOrig

      origTree = M.lookup t vertexTrees
      topComments = filter isCommentNode (maybe [] tMetaNodes origTree)

      sideCommentEntry :: [VertexTreeEntry]
      sideCommentEntry =
        [CommentEntry (sideComment t) | isNothing origTree && not (null groupsSorted)]

      finalEntries = sideCommentEntry ++ process (existingNames origTree) t M.empty groupsSorted
   in case nonEmpty finalEntries of
        Just ne -> Right $ VertexTree topComments ne
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
isMeta (MetaEntry _) = True
isMeta _ = False

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
sortCommentGroups = sortOn (\cg -> (cMeta cg, vZ (cVertex cg), vY (cVertex cg)))

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
   in case nonEmpty sortedGroups of
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
          (_, nodes') = mapAccumL updateVertexNames 0 (NE.fromList processedGroups)
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

transform :: Node -> Either Text Node
transform topNode =
  getVertexForest verticesQuery topNode
    >>= getNamesAndUpdateTree
  where
    getNamesAndUpdateTree (globals, vertexForest) =
      let vertexNames = getVertexNamesInForest vertexForest
       in updateVertexForest globals vertexForest
            >>= getUpdatedNamesAndUpdateGlobally vertexNames
    getUpdatedNamesAndUpdateGlobally oldVertexNames updatedVertexForest =
      let updatedVertexNames = getVertexNamesInForest updatedVertexForest
          updateMap = M.fromList $ on zip M.elems oldVertexNames updatedVertexNames
       in Right . findAndUpdateTextInNode updateMap newCursor $
            updateVerticesInNode verticesQuery updatedVertexForest topNode
