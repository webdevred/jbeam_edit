module Transformation (transform) where

import Config
import Control.Monad (foldM)
import Core.Node
import Core.NodeCursor (newCursor)
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Vector (Vector, (!), (!?), (//))
import Types
import VertexExtraction

import Core.NodeCursor qualified as NC
import Core.NodePath qualified as NP
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V

verticesQuery :: NP.NodePath
verticesQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "nodes"]

prefixForType :: VertexTreeType -> Text
prefixForType LeftTree = "l"
prefixForType MiddleTree = "m"
prefixForType RightTree = "r"
prefixForType SupportTree = ""

sideCommentText :: VertexTreeType -> Text
sideCommentText LeftTree = "Left side"
sideCommentText MiddleTree = "Middle side"
sideCommentText RightTree = "Right side"
sideCommentText SupportTree = "Support"

sideComment :: VertexTreeType -> InternalComment
sideComment t = InternalComment (sideCommentText t) False

renameVertexId :: VertexTreeType -> Int -> Text -> Text
renameVertexId treeType idx vertexName =
  let prefix = dropIndex vertexName
      cleanPrefix =
        if T.length prefix >= 3 && T.last prefix `elem` ['l', 'm', 'r']
          then T.init prefix
          else prefix
   in cleanPrefix <> prefixForType treeType <> show idx

buildTreeForType
  :: VertexForest -> VertexTreeType -> [AnnotatedVertex] -> Maybe VertexTree
buildTreeForType originalForest treeType groupsOrig =
  case nonEmpty groupsOrig of
    Just ne ->
      let origTree = M.lookup treeType originalForest
          topComments = maybe [] tComments origTree
          topComments' =
            if null topComments
              then sideComment treeType : topComments
              else topComments
       in Just $
            VertexTree topComments' (one ne)
    Nothing -> Nothing

addPrefixComments
  :: NonEmpty (NonEmpty AnnotatedVertex)
  -> NonEmpty (NonEmpty AnnotatedVertex)
addPrefixComments (x :| []) = one x
addPrefixComments (x :| xs) = x :| map addToCG xs
  where
    addToCG ((AnnotatedVertex comments vertex meta) :| cgs) =
      let commentName = dropIndex $ vName vertex
          newComment = InternalComment ("prefix group " <> commentName) False
       in AnnotatedVertex (newComment : comments) vertex meta :| cgs

addVertexTreeToForest
  :: Scientific
  -> Map VertexTreeType [AnnotatedVertex]
  -> VertexForest
  -> VertexForest
  -> VertexTreeType
  -> Either Text VertexForest
addVertexTreeToForest thr grouped forest forestAcc t =
  case M.lookup t grouped of
    Just groupsForT ->
      case buildTreeForType forest t groupsForT of
        Just vt ->
          let groupsToSort = tAnnotatedVertices vt
              groupsSorted = sconcat $ NE.map (sortVertices thr t) groupsToSort
              prefixCommentedGroups =
                addPrefixComments $ NE.groupWith1 (dropIndex . vName . aVertex) groupsSorted
              vt' = vt {tAnnotatedVertices = prefixCommentedGroups}
           in Right $ M.insert t vt' forestAcc
        Nothing -> Right forestAcc
    Nothing -> Right forestAcc

groupAnnotatedVertices
  :: XGroupBreakpoints
  -> AnnotatedVertex
  -> Maybe (VertexTreeType, [AnnotatedVertex])
groupAnnotatedVertices brks g = do
  treeType <- determineGroup brks (aVertex g)
  pure (treeType, [g])

moveVerticesInVertexForest
  :: XGroupBreakpoints -> Scientific -> VertexForest -> Either Text VertexForest
moveVerticesInVertexForest brks thr vertexTrees =
  let supportTree = M.lookup SupportTree vertexTrees
      movableTrees = M.delete SupportTree vertexTrees
      allVertices = concatMap (NE.toList . sconcat . tAnnotatedVertices) (M.elems movableTrees)
      movableVertices = filter (not . isSupportVertex . aVertex) allVertices
   in case mapM (groupAnnotatedVertices brks) movableVertices of
        Just movableVertices' -> do
          let groupedVertices = M.fromListWith (++) movableVertices'
          newForest <-
            foldM
              (addVertexTreeToForest thr groupedVertices vertexTrees)
              M.empty
              treesOrderNoSupport
          Right $ maybe newForest (\st -> M.insert SupportTree st newForest) supportTree
        Nothing -> Left "invalid breakpoint"

getVertexNamesInForest
  :: VertexForest -> M.Map (Scientific, Scientific, Scientific) Text
getVertexNamesInForest =
  M.unions
    . M.map
      ( \(VertexTree _ groups) ->
          M.fromList $
            map
              (\cg -> let v = aVertex cg in ((vX v, vY v, vZ v), vName v))
              (NE.toList $ sconcat groups)
      )

vertexTreeToNodesWithPrev
  :: MetaMap
  -> VertexTreeType
  -> VertexTree
  -> (MetaMap, NE.NonEmpty Node)
vertexTreeToNodesWithPrev prevMeta _ (VertexTree topComments groups) =
  let topNodes = NE.fromList $ map Comment topComments

      stepVertex pm av =
        let (nodes, newMeta) = commentGroupToNodesWithPrev pm av
         in (newMeta, NE.fromList nodes)

      stepGroup pm avGroup =
        let (finalMeta', nodesLists) = mapAccumL stepVertex pm avGroup
         in (finalMeta', sconcat nodesLists)

      (finalMeta, groupNodesLists) = mapAccumL stepGroup prevMeta groups

      allNodes = topNodes <> sconcat groupNodesLists
   in (finalMeta, allNodes)

removeIdenticalMeta :: MetaMap -> MetaMap -> MetaMap
removeIdenticalMeta = M.differenceWithKey diff
  where
    diff _ v1 v2
      | v1 == v2 = Nothing
      | otherwise = Just v1

commentGroupToNodesWithPrev
  :: MetaMap
  -> AnnotatedVertex
  -> ([Node], MetaMap)
commentGroupToNodesWithPrev prevMeta (AnnotatedVertex comments vertex meta) =
  let localsMeta = removeIdenticalMeta meta prevMeta

      newPrevMeta = M.union localsMeta prevMeta

      metaNodes =
        [ Object (V.singleton (ObjectKey (String k, v)))
        | (k, v) <- M.assocs localsMeta
        ]

      commentNodes :: [Node]
      commentNodes = map Comment comments

      vertexArray :: Node
      vertexArray =
        Array $
          V.fromList
            [ String (vName vertex)
            , Number (vX vertex)
            , Number (vY vertex)
            , Number (vZ vertex)
            ]
   in (commentNodes ++ metaNodes ++ [vertexArray], newPrevMeta)

vertexForestToNodeVector :: MetaMap -> VertexForest -> Vector Node
vertexForestToNodeVector initialMeta vf =
  let step prevMeta treeType =
        case M.lookup treeType vf of
          Nothing -> (prevMeta, [])
          Just tree ->
            let (prevMeta', nodes) = vertexTreeToNodesWithPrev prevMeta treeType tree
             in (prevMeta', NE.toList nodes)

      (_, listsOfNodes) = mapAccumL step initialMeta treesOrder
   in V.fromList . concat $ listsOfNodes

treesOrderNoSupport :: [VertexTreeType]
treesOrderNoSupport = [LeftTree, MiddleTree, RightTree]

treesOrder :: [VertexTreeType]
treesOrder = treesOrderNoSupport ++ [SupportTree]

compareCG :: Scientific -> AnnotatedVertex -> AnnotatedVertex -> Ordering
compareCG thr vertex1 vertex2 =
  let z1 = vZ . aVertex $ vertex1
      z2 = vZ . aVertex $ vertex2
      compareY = on compare (vY . aVertex)
      sortingFun =
        let zDiff = abs $ z1 - z2
         in if zDiff > thr
              then
                compare z1 z2
              else
                compareY vertex1 vertex2
   in on compare aMeta vertex1 vertex2 <> sortingFun

sortVertices
  :: Scientific
  -> VertexTreeType
  -> NonEmpty AnnotatedVertex
  -> NonEmpty AnnotatedVertex
sortVertices thr treeType groups =
  let sortedGroups = NE.sortBy (compareCG thr) groups

      assignNames idx cg =
        let v = aVertex cg
            newV = v {vName = renameVertexId treeType idx (vName v)}
         in (idx + 1, cg {aVertex = newV})

      renamedGroups =
        case treeType of
          SupportTree -> sortedGroups
          _ -> snd $ mapAccumL assignNames 0 sortedGroups
   in renamedGroups

updateVerticesInNode
  :: NP.NodePath -> VertexForest -> NE.NonEmpty Node -> Node -> Node
updateVerticesInNode (NP.NodePath Empty) g globals (Array _) =
  let globalsList = NE.toList globals
      initialMeta =
        M.unions (map metaMapFromObject (NE.toList globals))
   in Array (V.fromList globalsList <> vertexForestToNodeVector initialMeta g)
updateVerticesInNode (NP.NodePath ((NP.ArrayIndex i) :<| qrest)) g globals (Array children) =
  let updateInNode nodeToUpdate =
        children
          // [(i, updateVerticesInNode (NP.NodePath qrest) g globals nodeToUpdate)]
   in Array $ maybe children updateInNode (children !? i)
updateVerticesInNode (NP.NodePath ((NP.ObjectIndex i) :<| qrest)) g globals (Object children) =
  let updateInNode _ =
        children
          // [(i, updateVerticesInNode (NP.NodePath qrest) g globals (children ! i))]
   in Object $ maybe children updateInNode (children !? i)
updateVerticesInNode (NP.NodePath (k@(NP.ObjectKey _) :<| qrest)) g globals (Object children) =
  let updateInNode i =
        children
          // [(i, updateVerticesInNode (NP.NodePath qrest) g globals (children ! i))]
   in Object . maybe children updateInNode $ V.findIndex (isObjectKeyEqual k) children
updateVerticesInNode query g globals (ObjectKey (k, v)) =
  ObjectKey (k, updateVerticesInNode query g globals v)
updateVerticesInNode _ _ _ a = a

isObjectKeyEqual :: NP.NodeSelector -> Node -> Bool
isObjectKeyEqual (NP.ObjectKey a) (ObjectKey (String b, _)) = a == b
isObjectKeyEqual _ _ = False

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

transform :: TransformationConfig -> Node -> Either Text Node
transform (TransformationConfig thr brks) topNode =
  getVertexForest brks verticesQuery topNode
    >>= getNamesAndUpdateTree
  where
    getNamesAndUpdateTree (globals, vertexForest) =
      let vertexNames = getVertexNamesInForest vertexForest
       in moveVerticesInVertexForest brks thr vertexForest
            >>= getUpdatedNamesAndUpdateGlobally globals vertexNames
    getUpdatedNamesAndUpdateGlobally globals oldVertexNames updatedVertexForest =
      let updatedVertexNames = getVertexNamesInForest updatedVertexForest
          updateMap = M.fromList $ on zip M.elems oldVertexNames updatedVertexNames
       in Right . findAndUpdateTextInNode updateMap newCursor $
            updateVerticesInNode verticesQuery updatedVertexForest globals topNode
