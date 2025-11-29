module JbeamEdit.Transformation (transform) where

import Control.Monad (foldM)
import Data.List (partition)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Text qualified as T
import Data.Vector (Vector, (!), (!?), (//))
import Data.Vector qualified as V
import JbeamEdit.Core.Node
import JbeamEdit.Core.NodeCursor (newCursor)
import JbeamEdit.Core.NodeCursor qualified as NC
import JbeamEdit.Core.NodePath qualified as NP
import JbeamEdit.Transformation.Config
import JbeamEdit.Transformation.OMap1 (OMap1)
import JbeamEdit.Transformation.OMap1 qualified as OMap1
import JbeamEdit.Transformation.SupportVertex
import JbeamEdit.Transformation.Types
import JbeamEdit.Transformation.VertexExtraction

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

addSideComment
  :: Ord k
  => VertexTreeType -> Bool -> OMap1 k VertexTree -> OMap1 k VertexTree
addSideComment t False trees =
  let (key, VertexTree topComments vertices, otherTrees) = OMap1.uncons trees
      newComment = sideComment t
   in OMap1.consOMap (key, VertexTree (newComment : topComments) vertices) otherTrees
addSideComment _ True trees = trees

addPrefixComments
  :: VertexTreeType
  -> OMap1 VertexTreeKey VertexTree
  -> OMap1 VertexTreeKey VertexTree
addPrefixComments SupportTree trees = trees
addPrefixComments _ trees = bool trees (fmap addToAnnotatedVertex trees) (length trees > 1)
  where
    addToAnnotatedVertex (VertexTree [] namedVertexGroups) =
      let commentName = dropIndex . vName . aVertex . head $ namedVertexGroups
          newComment = InternalComment ("prefix group " <> commentName) False NextNode
       in VertexTree [newComment] namedVertexGroups
    addToAnnotatedVertex (VertexTree comments namedVertexGroups) = VertexTree comments namedVertexGroups

prefixForVertexKey
  :: Maybe (OMap1 VertexTreeKey VertexTree)
  -> NonEmpty AnnotatedVertex
  -> (VertexTreeKey, VertexTree)
prefixForVertexKey origTree vs =
  let firstAv = head vs
      firstVertex = aVertex firstAv
      prefixKey = PrefixKey . dropIndex $ vName firstVertex
      topComments = concatMap tComments (OMap1.lookup prefixKey =<< origTree)
   in (prefixKey, VertexTree topComments vs)

sortByKeyOrderNE
  :: Maybe (OMap1 VertexTreeKey VertexTree)
  -> NonEmpty (VertexTreeKey, VertexTree)
  -> NonEmpty (VertexTreeKey, VertexTree)
sortByKeyOrderNE original xs =
  let order = concatMap (map fst . OMap1.assocs) original
      rank :: Map VertexTreeKey Int
      rank = M.fromList (zip order [0 ..])
      fallback = length order
      compareFun (a, _) = fromMaybe fallback (M.lookup a rank)
   in NE.sortBy (on compare compareFun) xs

groupByPrefix
  :: Maybe (OMap1 VertexTreeKey VertexTree)
  -> NonEmpty AnnotatedVertex
  -> OMap1 VertexTreeKey VertexTree
groupByPrefix origTree =
  OMap1.fromNEList
    . sortByKeyOrderNE origTree
    . NE.map (prefixForVertexKey origTree)
    . NE.groupWith1 (dropIndex . vName . aVertex)

commentsExists :: Maybe (OMap1 VertexTreeKey VertexTree) -> Bool
commentsExists = not . all (null . tComments . OMap1.head)

addVertexTreeToForest
  :: UpdateNamesMap
  -> TransformationConfig
  -> Map VertexTreeType [AnnotatedVertex]
  -> VertexForest
  -> VertexForest
  -> VertexTreeType
  -> Either Text VertexForest
addVertexTreeToForest newNames tf grouped forest forestAcc t =
  case nonEmpty =<< M.lookup t grouped of
    Just groupsForT ->
      let origTree = M.lookup t forest
          tree =
            addSideComment t (commentsExists origTree)
              . addPrefixComments t
              . fmap (sortVertices t newNames tf)
              $ groupByPrefix origTree groupsForT
       in Right (M.insert t tree forestAcc)
    Nothing -> Right forestAcc

groupAnnotatedVertices
  :: XGroupBreakpoints
  -> AnnotatedVertex
  -> Maybe (VertexTreeType, [AnnotatedVertex])
groupAnnotatedVertices brks g = do
  treeType <- determineGroup' brks (aVertex g)
  pure (treeType, [g])

updateSupportVertexName
  :: VertexTreeType
  -> AnnotatedVertex
  -> AnnotatedVertex
updateSupportVertexName vType (AnnotatedVertex c v m) = AnnotatedVertex c (v {vName = newName}) m
  where
    name = vName v
    newName = dropIndex name <> prefixForType vType

moveSupportVertices
  :: UpdateNamesMap
  -> TransformationConfig
  -> VertexConnMap
  -> M.Map VertexTreeType [AnnotatedVertex]
  -> (VertexForest, M.Map VertexTreeType [AnnotatedVertex])
moveSupportVertices newNames tfCfg connMap vsPerType =
  let supportVertices :: [(VertexTreeType, AnnotatedVertex)]
      supportVertices =
        [ (vType, av)
        | (vType, vs) <- M.toList vsPerType
        , av <- vs
        , let name = vName (aVertex av)
        , let vertexCount = length vs
              thrCount =
                max 1 (round $ supportThreshold tfCfg / 100 * fromIntegral vertexCount)
        , Just (_bestType, count) <- [M.lookup name connMap]
        , count >= thrCount
        ]

      brks = xGroupBreakpoints tfCfg
      thr = zSortingThreshold tfCfg

      assignSupportNames = assignNames newNames brks SupportTree

      vertexForest :: VertexForest
      vertexForest =
        case nonEmpty supportVertices of
          Nothing -> M.empty
          Just vs ->
            one
              ( SupportTree
              , OMap1.singleton
                  ( SupportKey
                  , VertexTree
                      [sideComment SupportTree]
                      ( snd
                          . mapAccumL
                            assignSupportNames
                            M.empty
                          . NE.sortBy (compareAV thr SupportTree)
                          $ NE.map (uncurry updateSupportVertexName) vs
                      )
                  )
              )

      remainingVertices :: M.Map VertexTreeType [AnnotatedVertex]
      remainingVertices =
        M.map (filter (`notElemByVertexName` map snd supportVertices)) vsPerType
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
  let allVertices = concatMap (concatMap (NE.toList . tAnnotatedVertices) . toList) vertexTrees
      brks = xGroupBreakpoints tfCfg
   in case mapM (groupAnnotatedVertices brks) allVertices of
        Just movableVertices' -> do
          let groupedVertices = M.fromListWith (++) movableVertices'
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
      ( M.fromList
          . map
            (\av -> let v = aVertex av in ((vX v, vY v, vZ v), vName v))
          . concatMap (toList . tAnnotatedVertices)
          . toList
      )

vertexTreeToNodesWithPrev
  :: MetaMap
  -> VertexTreeType
  -> VertexTree
  -> (MetaMap, NonEmpty Node)
vertexTreeToNodesWithPrev prevMeta _ (VertexTree topComments groups) =
  let topNodes = map Comment topComments

      stepVertex pm av =
        let (nodes, newMeta) = annotatedVertexToNodesWithPrev pm av
         in (newMeta, NE.fromList nodes)

      (finalMeta, groupNodesLists) = mapAccumL stepVertex prevMeta groups

      allNodes = topNodes `NE.prependList` sconcat groupNodesLists
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
  -> ([Node], MetaMap)
annotatedVertexToNodesWithPrev prevMeta (AnnotatedVertex comments vertex meta) =
  let localsMeta = removeIdenticalMeta meta prevMeta

      newPrevMeta = M.union localsMeta prevMeta

      metaNodes =
        [ Object (V.singleton (ObjectKey (String k, v)))
        | (k, v) <- M.assocs localsMeta
        ]

      (postComments, preComments) = partition commentIsAttachedToPreviousNode comments

      vertexArray :: Node
      vertexArray =
        let name = String (vName vertex)
            x = Number (vX vertex)
            y = Number (vY vertex)
            z = Number (vZ vertex)
            possiblyMeta = concatMap (one . Object) (vMeta vertex)
         in Array . V.fromList $ [name, x, y, z] ++ possiblyMeta
   in ( map Comment preComments
          ++ metaNodes
          ++ one vertexArray
          ++ map Comment postComments
      , newPrevMeta
      )

vertexForestToNodeVector :: MetaMap -> VertexForest -> Vector Node
vertexForestToNodeVector initialMeta vf =
  let stepType prevMeta treeType =
        case M.lookup treeType vf of
          Nothing -> (prevMeta, [])
          Just oMap ->
            foldl'
              ( \(pm, accNodes) tree ->
                  let (pm', nodes) = vertexTreeToNodesWithPrev pm treeType tree
                   in (pm', accNodes ++ NE.toList nodes)
              )
              (prevMeta, [])
              (toList oMap)

      (_, listsOfNodes) = mapAccumL stepType initialMeta treesOrder
   in V.fromList (concat listsOfNodes)

treesOrder :: [VertexTreeType]
treesOrder = [LeftTree, MiddleTree, RightTree, SupportTree]

compareAV
  :: Scientific -> VertexTreeType -> AnnotatedVertex -> AnnotatedVertex -> Ordering
compareAV thr treeType vertex1 vertex2 =
  let supportNameCompare =
        bool
          EQ
          (on compare (dropIndex . vName . aVertex) vertex1 vertex2)
          (treeType == SupportTree)
      y1 = vY . aVertex $ vertex1
      y2 = vY . aVertex $ vertex2
      compareZ = on compare (vZ . aVertex) vertex1 vertex2
      compareY =
        let zDiff = abs $ y1 - y2
         in bool EQ (compare y1 y2) (zDiff > thr)
      compareX = on compare (vX . aVertex) vertex1 vertex2
   in mconcat
        [ supportNameCompare
        , on compare aMeta vertex1 vertex2
        , compareY
        , compareZ
        , compareX
        ]

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
      isLmr = lastChar `elem` ['l', 'm', 'r']
      supportPrefixChar = one 's' <> bool typeSpecific (one lastChar) isLmr
      cleanPrefix
        | treeType /= SupportTree
            && T.length prefix >= 3
            && T.last prefix' == 's' =
            updatedPrefix (T.init prefix') <> typeSpecific
        | treeType /= SupportTree
            && T.length prefix >= 3
            && isLmr =
            updatedPrefix prefix' <> typeSpecific
        | treeType /= SupportTree =
            updatedPrefix prefix <> typeSpecific
        | T.length prefix' >= 3
            && T.last prefix' == 's' =
            updatedPrefix (T.init prefix') <> one 's' <> typeSpecific
        | T.length prefix' < 2 =
            updatedPrefix prefix <> supportPrefixChar
        | otherwise =
            updatedPrefix prefix' <> supportPrefixChar
      lastIdx = M.findWithDefault 0 cleanPrefix prefixMap
      newName = renameVertexId treeType lastIdx cleanPrefix
      newVertex = v {vName = newName}
      prefixMap' = M.insert cleanPrefix (lastIdx + 1) prefixMap
   in (prefixMap', av {aVertex = newVertex})

sortVertices
  :: VertexTreeType
  -> UpdateNamesMap
  -> TransformationConfig
  -> VertexTree
  -> VertexTree
sortVertices treeType newNames tfCfg (VertexTree comments vertices) =
  let thr = zSortingThreshold tfCfg
      brks = xGroupBreakpoints tfCfg
      sortedGroups = NE.sortBy (compareAV thr treeType) vertices

      renamedGroups = snd $ mapAccumL (assignNames newNames brks treeType) M.empty sortedGroups
   in VertexTree comments renamedGroups

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

findAndUpdateTextInNode :: UpdateNamesMap -> NC.NodeCursor -> Node -> Node
findAndUpdateTextInNode m cursor node =
  case node of
    Array arr
      | NC.comparePathAndCursor verticesQuery cursor -> Array arr
      | otherwise -> Array $ V.imap applyBreadcrumbAndUpdateText arr
    Object obj -> Object $ V.imap applyBreadcrumbAndUpdateText obj
    ObjectKey (key, value) ->
      ObjectKey
        (key, findAndUpdateTextInNode m cursor value)
    String s -> String $ M.findWithDefault s s m
    _ -> node
  where
    applyBreadcrumbAndUpdateText =
      NC.applyCrumb cursor (findAndUpdateTextInNode m)

transform
  :: UpdateNamesMap
  -> TransformationConfig
  -> Node
  -> Either Text ([Node], [Node], Node)
transform newNames tfCfg topNode =
  getVertexForest (xGroupBreakpoints tfCfg) verticesQuery topNode
    >>= getNamesAndUpdateTree
  where
    getNamesAndUpdateTree (badNodes, globals, vertexForest) =
      let vertexNames = getVertexNamesInForest vertexForest
       in moveVerticesInVertexForest topNode newNames tfCfg vertexForest
            >>= getUpdatedNamesAndUpdateGlobally badNodes globals vertexNames
    getUpdatedNamesAndUpdateGlobally badVertexNodes globals oldVertexNames (badBeamNodes, updatedVertexForest) =
      let updatedVertexNames = getVertexNamesInForest updatedVertexForest
          updateMap = M.fromList $ on zip M.elems oldVertexNames updatedVertexNames
          newTopNode =
            findAndUpdateTextInNode updateMap newCursor $
              updateVerticesInNode verticesQuery updatedVertexForest globals topNode
       in Right (badVertexNodes, badBeamNodes, newTopNode)
