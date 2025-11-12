module Transformation (transform) where

import Config
import Control.Monad (foldM)
import Core.Node
import Core.NodeCursor (newCursor)
import Core.NodeCursor qualified as NC
import Core.NodePath qualified as NP
import Data.Map qualified as M
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Text qualified as T
import Data.Vector (Vector, (!), (!?), (//))
import Data.Vector qualified as V
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEV
import SupportVertex
import Types
import VectorHelpers
import VectorHelpers qualified as VH
import VertexExtraction

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
sideComment t = InternalComment (sideCommentText t) False NextNode

buildTreeForType
  :: VertexForest
  -> VertexTreeType
  -> Vector AnnotatedVertex
  -> Maybe VertexTree
buildTreeForType originalForest treeType groupsOrig =
  NEV.fromVector groupsOrig >>= go
  where
    go ne =
      let origTree = M.lookup treeType originalForest
          topComments = maybe V.empty tComments origTree
          topComments' =
            if null topComments
              then V.singleton $ sideComment treeType
              else topComments
       in ( Just $
              VertexTree topComments' (NEV.singleton ne)
          )

addPrefixComments
  :: NonEmptyVector
       (NonEmptyVector AnnotatedVertex)
  -> NonEmptyVector
       (NonEmptyVector AnnotatedVertex)
addPrefixComments allAvGroups =
  let (avGroup, avGroups) = NEV.uncons allAvGroups
   in avGroup `NEV.consV` V.map addToAnnotatedVertex avGroups
  where
    addToAnnotatedVertex avs =
      let commentName = dropIndex $ vName vertex
          (AnnotatedVertex comments vertex meta, innerAvs) = NEV.uncons avs
          newComment = pure $ InternalComment ("prefix group " <> commentName) False NextNode
       in AnnotatedVertex (newComment <> comments) vertex meta `NEV.consV` innerAvs

groupByPrefix
  :: NonEmptyVector AnnotatedVertex
  -> NonEmptyVector
       (NonEmptyVector AnnotatedVertex)
groupByPrefix = VH.groupWith1 (dropIndex . vName . aVertex)

addVertexTreeToForest
  :: UpdateNamesMap
  -> TransformationConfig
  -> Map VertexTreeType (Vector AnnotatedVertex)
  -> VertexForest
  -> VertexForest
  -> VertexTreeType
  -> Either Text VertexForest
addVertexTreeToForest newNames tf grouped forest forestAcc t =
  case M.lookup t grouped of
    Just groupsForT ->
      let tree = buildTreeForType forest t groupsForT
       in case tree of
            Just vt ->
              let groupsToSort = tAnnotatedVertices vt
                  groupsSorted =
                    if t /= SupportTree
                      then
                        foldMap
                          (NEV.toVector . sortVertices t newNames tf)
                          (NEV.toVector groupsToSort)
                      else
                        foldMap NEV.toVector groupsToSort
               in case NEV.fromVector groupsSorted of
                    Just groupsSorted' ->
                      let prefixCommentedGroups =
                            addPrefixComments . groupByPrefix $ groupsSorted'
                          vt' = vt {tAnnotatedVertices = prefixCommentedGroups}
                       in Right $ M.insert t vt' forestAcc
                    Nothing -> Right forestAcc
            Nothing -> Right forestAcc
    Nothing -> Right forestAcc

groupAnnotatedVertices
  :: XGroupBreakpoints
  -> AnnotatedVertex
  -> Maybe (VertexTreeType, Vector AnnotatedVertex)
groupAnnotatedVertices brks g = do
  treeType <- determineGroup' brks (aVertex g)
  pure (treeType, pure g)

updateSupportVertexName
  :: VertexTreeType
  -> AnnotatedVertex
  -> AnnotatedVertex
updateSupportVertexName vType (AnnotatedVertex c v m) = AnnotatedVertex c (v {vName = newName}) m
  where
    name = vName v
    newName =
      case T.unsnoc name of
        Nothing -> name
        Just (prefix, _) -> prefix <> prefixForType vType

moveSupportVertices
  :: UpdateNamesMap
  -> TransformationConfig
  -> VertexConnMap
  -> M.Map VertexTreeType (Vector AnnotatedVertex)
  -> (VertexForest, Map VertexTreeType (Vector AnnotatedVertex))
moveSupportVertices newNames tfCfg connMap vsPerType =
  let supportVertices :: [(VertexTreeType, AnnotatedVertex)]
      supportVertices =
        [ (vType, av)
        | (vType, vs) <- M.toList vsPerType
        , av <- toList vs
        , let name = vName (aVertex av)
        , let vertexCount = length vs
              thrCount =
                max 1 (round $ supportThreshold tfCfg / 100 * fromIntegral vertexCount)
        , Just (_bestType, count) <- [M.lookup name connMap]
        , count >= thrCount
        ]

      vertexForest :: VertexForest
      vertexForest =
        case NEV.fromList supportVertices of
          Nothing -> M.empty
          Just vs ->
            one
              ( SupportTree
              , VertexTree
                  (V.singleton $ sideComment SupportTree)
                  ( NEV.singleton $
                      sortSupportVertices
                        newNames
                        tfCfg
                        (NEV.map (uncurry updateSupportVertexName) vs)
                  )
              )

      remainingVertices :: M.Map VertexTreeType (Vector AnnotatedVertex)
      remainingVertices =
        M.map
          (V.filter (`notElemByVertexName` V.map snd (V.fromList supportVertices)))
          vsPerType
   in (vertexForest, remainingVertices)

notElemByVertexName
  :: Foldable t
  => AnnotatedVertex -> t AnnotatedVertex -> Bool
notElemByVertexName vertex = not . any (on (==) (vName . aVertex) vertex)

moveVerticesInVertexForest
  :: Node
  -> UpdateNamesMap
  -> TransformationConfig
  -> VertexForest
  -> Either Text ([Node], VertexForest)
moveVerticesInVertexForest topNode newNames tfCfg vertexTrees =
  let allVertices = concatMap (toList . NEV.toVector . fold1 . tAnnotatedVertices) vertexTrees
      brks = xGroupBreakpoints tfCfg
   in case mapM (groupAnnotatedVertices brks) allVertices of
        Just movableVertices' -> do
          let groupedVertices = M.fromListWith (<>) movableVertices'
          (badBeamNodes, conns) <-
            vertexConns (maxSupportCoordinates tfCfg) topNode groupedVertices
          let (supportForest, nonSupportVertices) =
                moveSupportVertices
                  newNames
                  tfCfg
                  conns
                  groupedVertices
          newForest <-
            foldM
              (addVertexTreeToForest newNames tfCfg nonSupportVertices vertexTrees)
              supportForest
              treesOrder
          Right (badBeamNodes, newForest)
        Nothing -> Left "invalid breakpoint"

getVertexNamesInForest
  :: VertexForest -> M.Map (Scientific, Scientific, Scientific) Text
getVertexNamesInForest =
  M.unions
    . M.map
      ( \(VertexTree _ groups) ->
          M.fromList $
            map
              (\av -> let v = aVertex av in ((vX v, vY v, vZ v), vName v))
              (NEV.toList $ VH.fold1 groups)
      )

vertexTreeToNodesWithPrev
  :: MetaMap
  -> VertexTreeType
  -> VertexTree
  -> (MetaMap, NonEmptyVector Node)
vertexTreeToNodesWithPrev prevMeta _ (VertexTree topComments groups) =
  let topNodes = V.map Comment topComments

      stepVertex pm av =
        let (nodes, newMeta) = annotatedVertexToNodesWithPrev pm av
         in (newMeta, NEV.unsafeFromVector nodes)

      stepGroup pm avGroup =
        let (finalMeta', nodesLists) = mapAccumL stepVertex pm avGroup
         in (finalMeta', VH.fold1 nodesLists)

      (finalMeta, groupNodesLists) = mapAccumL stepGroup prevMeta groups

      allNodes = NEV.unsafeFromVector $ topNodes <> NEV.toVector (VH.fold1 groupNodesLists)
   in (finalMeta, allNodes)

removeIdenticalMeta :: MetaMap -> MetaMap -> MetaMap
removeIdenticalMeta = M.differenceWithKey diff
  where
    diff _ v1 v2
      | v1 == v2 = Nothing
      | otherwise = Just v1

annotatedVertexToNodesWithPrev
  :: MetaMap
  -> AnnotatedVertex
  -> (Vector Node, MetaMap)
annotatedVertexToNodesWithPrev prevMeta (AnnotatedVertex comments vertex meta) =
  let localsMeta = removeIdenticalMeta meta prevMeta

      newPrevMeta = M.union localsMeta prevMeta

      metaNodes =
        V.fromList
          [ Object (V.singleton (ObjectKey (String k, v)))
          | (k, v) <- M.assocs localsMeta
          ]

      (postComments, preComments) = V.partition commentIsAttachedToPreviousNode comments

      vertexArray :: Node
      vertexArray =
        let name = String (vName vertex)
            x = Number (vX vertex)
            y = Number (vY vertex)
            z = Number (vZ vertex)
            possiblyMeta = maybe [] (one . Object) (vMeta vertex)
         in Array . V.fromList $ [name, x, y, z] ++ possiblyMeta
   in ( V.map Comment preComments
          <> metaNodes
          <> V.singleton vertexArray
          <> V.map Comment postComments
      , newPrevMeta
      )

vertexForestToNodeVector :: MetaMap -> VertexForest -> Vector Node
vertexForestToNodeVector initialMeta vf =
  let step prevMeta treeType =
        case M.lookup treeType vf of
          Nothing -> (prevMeta, [])
          Just tree ->
            let (prevMeta', nodes) = vertexTreeToNodesWithPrev prevMeta treeType tree
             in (prevMeta', NEV.toList nodes)

      (_, listsOfNodes) = mapAccumL step initialMeta treesOrder
   in V.fromList . concat $ listsOfNodes

treesOrder :: [VertexTreeType]
treesOrder = [LeftTree, MiddleTree, RightTree, SupportTree]

compareAV
  :: Scientific -> VertexTreeType -> AnnotatedVertex -> AnnotatedVertex -> Ordering
compareAV thr treeType vertex1 vertex2 =
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
        | T.length prefix' < 2 =
            updatedPrefix prefix <> one 's' <> typeSpecific
        | otherwise =
            updatedPrefix prefix' <> one 's' <> typeSpecific
      lastIdx = M.findWithDefault 0 cleanPrefix prefixMap
      newName = renameVertexId treeType lastIdx cleanPrefix
      newVertex = v {vName = newName}
      prefixMap' = M.insert cleanPrefix (lastIdx + 1) prefixMap
   in (prefixMap', av {aVertex = newVertex})

sortSupportVertices
  :: UpdateNamesMap
  -> TransformationConfig
  -> NonEmptyVector AnnotatedVertex
  -> NonEmptyVector AnnotatedVertex
sortSupportVertices = sortVertices SupportTree

sortVertices
  :: VertexTreeType
  -> UpdateNamesMap
  -> TransformationConfig
  -> NonEmptyVector AnnotatedVertex
  -> NonEmptyVector AnnotatedVertex
sortVertices treeType newNames tfCfg groups =
  let thr = zSortingThreshold tfCfg
      brks = xGroupBreakpoints tfCfg
      groups' =
        if treeType /= SupportTree
          then
            VH.fold1 . groupByPrefix $ groups
          else groups
      sortedGroups = VH.sortBy (compareAV thr treeType) groups'

      renamedGroups =
        snd $ mapAccumL (assignNames newNames brks treeType) M.empty sortedGroups
   in renamedGroups

updateVerticesInNode
  :: NP.NodePath
  -> VertexForest
  -> NonEmptyVector Node
  -> Node
  -> Node
updateVerticesInNode (NP.NodePath Empty) g globals (Array _) =
  let initialMeta =
        M.unions (toList $ V.map metaMapFromObject (NEV.toVector globals))
   in Array (NEV.toVector globals <> vertexForestToNodeVector initialMeta g)
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

findAndUpdateTextInNode :: UpdateNamesMap -> NC.NodeCursor -> Node -> Node
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

transform
  :: UpdateNamesMap
  -> TransformationConfig
  -> Node
  -> Either Text ([Node], [Node], Node)
transform newNames tfCfg topNode =
  getVertexForest (xGroupBreakpoints tfCfg) verticesQuery topNode
    >>= getNamesAndUpdateTree
  where
    getNamesAndUpdateTree (badVertexNodes, globals, vertexForest) =
      let vertexNames = getVertexNamesInForest vertexForest
       in moveVerticesInVertexForest topNode newNames tfCfg vertexForest
            >>= getUpdatedNamesAndUpdateGlobally badVertexNodes globals vertexNames
    getUpdatedNamesAndUpdateGlobally badVertexNodes globals oldVertexNames (badBeamNodes, updatedVertexForest) =
      let updatedVertexNames = getVertexNamesInForest updatedVertexForest
          updateMap = M.fromList $ on zip M.elems oldVertexNames updatedVertexNames
          newTopNode =
            findAndUpdateTextInNode updateMap newCursor $
              updateVerticesInNode verticesQuery updatedVertexForest globals topNode
       in Right (toList badVertexNodes, badBeamNodes, newTopNode)
