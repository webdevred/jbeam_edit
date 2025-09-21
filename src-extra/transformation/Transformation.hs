module Transformation (transform) where

import Config
import Control.Monad (foldM)
import Core.Node
import Core.NodeCursor (newCursor)
import Data.List (partition)
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Vector (Vector, (!), (!?), (//))
import SupportVertex
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
sideCommentText SupportTree = "Support nodes"

sideComment :: VertexTreeType -> InternalComment
sideComment t = InternalComment (sideCommentText t) False False

buildTreeForType
  :: VertexConnMap
  -> Double
  -> VertexForest
  -> VertexTreeType
  -> [AnnotatedVertex]
  -> ([AnnotatedVertex], Maybe VertexTree)
buildTreeForType conns supThr originalForest treeType groupsOrig =
  let annVertexCount = round $ supThr * fromIntegral (length groupsOrig) / 100
      isSupportVertex' v = any (>= annVertexCount) $ M.lookup (vName . aVertex $ v) conns
      (supportVertices, nonSupportVertices) = partition isSupportVertex' groupsOrig
   in case nonEmpty nonSupportVertices of
        Just ne ->
          let origTree = M.lookup treeType originalForest
              topComments = maybe [] tComments origTree
              topComments' =
                if null topComments
                  then sideComment treeType : topComments
                  else topComments
           in ( supportVertices
              , Just $
                  VertexTree topComments' (one ne)
              )
        Nothing -> (supportVertices, Nothing)

addPrefixComments
  :: NonEmpty (NonEmpty AnnotatedVertex)
  -> NonEmpty (NonEmpty AnnotatedVertex)
addPrefixComments (x :| []) = one x
addPrefixComments (x :| xs) = x :| map addToCG xs
  where
    addToCG ((AnnotatedVertex comments vertex meta) :| cgs) =
      let commentName = dropIndex $ vName vertex
          newComment = InternalComment ("prefix group " <> commentName) False False
       in AnnotatedVertex (newComment : comments) vertex meta :| cgs

addSupportVertex
  :: AnnotatedVertex
  -> VertexForest
  -> VertexForest
addSupportVertex supportVertex =
  let supportSideComment = [sideComment SupportTree]
   in M.insertWith
        combineTrees
        SupportTree
        (VertexTree supportSideComment (one (one supportVertex)))

addSupportVertices
  :: VertexForest
  -> [AnnotatedVertex]
  -> VertexForest
addSupportVertices = foldr addSupportVertex

groupByPrefix
  :: NonEmpty AnnotatedVertex
  -> NonEmpty (NonEmpty AnnotatedVertex)
groupByPrefix = NE.groupWith1 (dropIndex . vName . aVertex)

addVertexTreeToForest
  :: Map Text Text
  -> TransformationConfig
  -> Map Text Int
  -> Map VertexTreeType [AnnotatedVertex]
  -> VertexForest
  -> VertexForest
  -> VertexTreeType
  -> Either Text VertexForest
addVertexTreeToForest newNames tf conns grouped forest forestAcc t =
  let supThr = supportThreshold tf
   in case M.lookup t grouped of
        Just groupsForT ->
          let (supportVertices, tree) = buildTreeForType conns supThr forest t groupsForT
           in case tree of
                Just vt ->
                  let groupsToSort = tAnnotatedVertices vt
                      groupsSorted =
                        if t /= SupportTree
                          then
                            concatMap
                              (NE.toList . sortVertices newNames tf t)
                              (NE.toList groupsToSort)
                          else
                            concatMap NE.toList groupsToSort
                   in case nonEmpty groupsSorted of
                        Just groupsSorted' ->
                          let prefixCommentedGroups =
                                addPrefixComments . groupByPrefix $ groupsSorted'
                              vt' = vt {tAnnotatedVertices = prefixCommentedGroups}
                           in Right $ addSupportVertices (M.insert t vt' forestAcc) supportVertices
                        Nothing -> Right $ addSupportVertices forestAcc supportVertices
                Nothing -> Right $ addSupportVertices forestAcc supportVertices
        Nothing -> Right forestAcc

groupAnnotatedVertices
  :: XGroupBreakpoints
  -> AnnotatedVertex
  -> Maybe (VertexTreeType, [AnnotatedVertex])
groupAnnotatedVertices brks g = do
  treeType <- determineGroup' brks (aVertex g)
  pure (treeType, [g])

sortSupportVertices
  :: UpdateNamesMap
  -> TransformationConfig
  -> VertexForest
  -> VertexForest
sortSupportVertices newNames tfCfg =
  M.update
    maybeNewTree
    SupportTree
  where
    maybeNewTree (VertexTree topComments supportVertices) =
      let vertices =
            one
              . sortVertices newNames tfCfg SupportTree
              . sconcat
              $ supportVertices
       in Just
            ( VertexTree
                topComments
                vertices
            )

moveVerticesInVertexForest
  :: UpdateNamesMap
  -> TransformationConfig
  -> VertexForest
  -> VertexConnMap
  -> Either Text VertexForest
moveVerticesInVertexForest newNames tfCfg vertexTrees conns =
  let allVertices = concatMap (NE.toList . sconcat . tAnnotatedVertices) vertexTrees
      brks = xGroupBreakpoints tfCfg
   in case mapM (groupAnnotatedVertices brks) allVertices of
        Just movableVertices' -> do
          let groupedVertices = M.fromListWith (++) movableVertices'
          newForest <-
            foldM
              (addVertexTreeToForest newNames tfCfg conns groupedVertices vertexTrees)
              M.empty
              treesOrder
          Right $ sortSupportVertices newNames tfCfg newForest
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

      (postComments, preComments) = partition assocWithPrior comments

      vertexArray :: Node
      vertexArray =
        let name = String (vName vertex)
            x = Number (vX vertex)
            y = Number (vY vertex)
            z = Number (vZ vertex)
            possiblyMeta = maybe [] (one . Object) (vMeta vertex)
         in Array . V.fromList $ [name, x, y, z] ++ possiblyMeta
   in ( map Comment preComments
          ++ metaNodes
          ++ one vertexArray
          ++ map Comment postComments
      , newPrevMeta
      )

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

treesOrder :: [VertexTreeType]
treesOrder = [LeftTree, MiddleTree, RightTree, SupportTree]

compareCG
  :: Scientific -> VertexTreeType -> AnnotatedVertex -> AnnotatedVertex -> Ordering
compareCG thr treeType vertex1 vertex2 =
  let supportNameCompare =
        bool
          EQ
          (on compare (dropIndex . vName . aVertex) vertex1 vertex2)
          (treeType /= SupportTree)
      y1 = vY . aVertex $ vertex1
      y2 = vY . aVertex $ vertex2
      compareZ = on compare (vZ . aVertex) vertex1 vertex2
      compareY =
        let zDiff = abs $ y1 - y2
         in bool EQ (compare y1 y2) (zDiff > thr)
   in supportNameCompare <> on compare aMeta vertex1 vertex2 <> compareY <> compareZ

renameVertexId :: VertexTreeType -> Int -> Text -> Text
renameVertexId treeType idx vertexPrefix =
  let idx' = bool "" (show idx) (treeType /= SupportTree || idx /= 0)
   in vertexPrefix <> idx'

assignNames
  :: UpdateNamesMap
  -> XGroupBreakpoints
  -> VertexTreeType
  -> Map Text Int
  -> AnnotatedVertex
  -> (Map Text Int, AnnotatedVertex)
assignNames newNames brks treeType prefixMap av =
  let v = aVertex av
      updatedPrefix cleanPrefix' = M.findWithDefault cleanPrefix' cleanPrefix' newNames
      prefix = dropIndex (vName v)
      typeSpecific = maybe "" prefixForType (determineGroup brks v)
      (prefix', lastChar) = fromMaybe (error "unreachable") (T.unsnoc prefix)
      cleanPrefix
        | treeType /= SupportTree
            && T.length prefix >= 3
            && T.last prefix' == 's' =
            updatedPrefix (T.init prefix') <> typeSpecific
        | treeType /= SupportTree
            && T.length prefix >= 3
            && lastChar `elem` ['l', 'm', 'r'] =
            updatedPrefix prefix' <> typeSpecific
        | treeType /= SupportTree =
            updatedPrefix prefix <> typeSpecific
        | T.length prefix' >= 3
            && T.last prefix' == 's' =
            updatedPrefix (T.init prefix') <> one 's' <> typeSpecific
        | otherwise =
            updatedPrefix prefix <> one 's' <> typeSpecific
      lastIdx = M.findWithDefault 0 cleanPrefix prefixMap
      newName = renameVertexId treeType lastIdx cleanPrefix
      newVertex = v {vName = newName}
      prefixMap' = M.insert cleanPrefix (lastIdx + 1) prefixMap
   in (prefixMap', av {aVertex = newVertex})

sortVertices
  :: UpdateNamesMap
  -> TransformationConfig
  -> VertexTreeType
  -> NonEmpty AnnotatedVertex
  -> NonEmpty AnnotatedVertex
sortVertices newNames tfCfg treeType groups =
  let thr = zSortingThreshold tfCfg
      brks = xGroupBreakpoints tfCfg
      groups' =
        if treeType /= SupportTree
          then
            sconcat . groupByPrefix $ groups
          else groups
      sortedGroups = NE.sortBy (compareCG thr treeType) groups'

      renamedGroups =
        snd $ mapAccumL (assignNames newNames brks treeType) M.empty sortedGroups
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

transform :: UpdateNamesMap -> TransformationConfig -> Node -> Either Text Node
transform newNames tfCfg topNode =
  getVertexForest (xGroupBreakpoints tfCfg) verticesQuery topNode
    >>= getNamesAndUpdateTree
  where
    getVertexConns = vertexConns topNode
    getNamesAndUpdateTree (globals, vertexForest) =
      let vertexNames = getVertexNamesInForest vertexForest
       in getVertexConns
            >>= moveVerticesInVertexForest newNames tfCfg vertexForest
            >>= getUpdatedNamesAndUpdateGlobally globals vertexNames
    getUpdatedNamesAndUpdateGlobally globals oldVertexNames updatedVertexForest =
      let updatedVertexNames = getVertexNamesInForest updatedVertexForest
          updateMap = M.fromList $ on zip M.elems oldVertexNames updatedVertexNames
       in Right . findAndUpdateTextInNode updateMap newCursor $
            updateVerticesInNode verticesQuery updatedVertexForest globals topNode
