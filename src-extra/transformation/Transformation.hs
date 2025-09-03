module Transformation (transform) where

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
        if not (T.null prefix) && T.last prefix `elem` ['l', 'm', 'r']
          then T.init prefix
          else prefix
   in cleanPrefix <> prefixForType treeType <> show idx

buildTreeForType
  :: VertexForest -> VertexTreeType -> [CommentGroup] -> Maybe VertexTree
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
            VertexTree topComments' ne
    Nothing -> Nothing

moveVerticesInVertexForest :: VertexForest -> Either Text VertexForest
moveVerticesInVertexForest vertexTrees = do
  let supportTree = M.lookup SupportTree vertexTrees
      movableTrees = M.delete SupportTree vertexTrees
      allGroups = concatMap (NE.toList . tCommentGroups) (M.elems movableTrees)
      movableGroups = filter (not . isSupportVertex . cVertex) allGroups
      grouped = M.fromListWith (++) [(determineGroup (cVertex g), [g]) | g <- movableGroups]

  newForest <-
    foldM
      ( \acc t ->
          case M.lookup t grouped of
            Just groupsForT ->
              case buildTreeForType vertexTrees t groupsForT of
                Just vt -> Right $ M.insert t vt acc
                Nothing -> Right acc
            Nothing -> Right acc
      )
      M.empty
      treesOrderNoSupport

  let finalForest = maybe newForest (\st -> M.insert SupportTree st newForest) supportTree
  pure finalForest

getVertexNamesInForest
  :: VertexForest -> M.Map (Scientific, Scientific, Scientific) Text
getVertexNamesInForest =
  M.unions
    . M.map
      ( \(VertexTree _ groups) ->
          M.fromList $
            map
              (\cg -> let v = cVertex cg in ((vX v, vY v, vZ v), vName v))
              (NE.toList groups)
      )

vertexTreeToNodes :: VertexTreeType -> VertexTree -> [Node]
vertexTreeToNodes treeType (VertexTree topComments groups) =
  map Comment topComments
    ++ concatMap
      snd
      (scanl processGroup (M.empty, []) (sortVertices treeType $ NE.toList groups))
  where
    processGroup :: (MetaMap, [Node]) -> CommentGroup -> (MetaMap, [Node])
    processGroup (prevMeta, _) cg =
      let (nodesForCg, newMeta) = commentGroupToNodesWithPrev prevMeta cg
       in (newMeta, nodesForCg)

commentGroupToNodesWithPrev :: MetaMap -> CommentGroup -> ([Node], MetaMap)
commentGroupToNodesWithPrev prevMeta (CommentGroup comments vertex meta) =
  let changedMeta = M.differenceWith diff meta prevMeta
      diff new old = if new == old then Nothing else Just new

      metaNodes =
        [Object (V.fromList [ObjectKey (String k, v)]) | (k, v) <- M.assocs changedMeta]
      commentNodes = map Comment comments
      vertexArray =
        Array $
          V.fromList
            [ String (vName vertex)
            , Number (vX vertex)
            , Number (vY vertex)
            , Number (vZ vertex)
            ]
   in (metaNodes ++ commentNodes ++ [vertexArray], meta)

treesOrderNoSupport :: [VertexTreeType]
treesOrderNoSupport = [LeftTree, MiddleTree, RightTree]

treesOrder :: [VertexTreeType]
treesOrder = treesOrderNoSupport ++ [SupportTree]

vertexForestToNodeVector :: VertexForest -> Vector Node
vertexForestToNodeVector vf =
  V.fromList $
    concatMap (\t -> maybe [] (vertexTreeToNodes t) (M.lookup t vf)) treesOrder

sortVertices :: VertexTreeType -> [CommentGroup] -> [CommentGroup]
sortVertices treeType groups =
  let sortedGroups =
        sortOn
          (\cg -> (cMeta cg, vZ (cVertex cg), vY (cVertex cg), vX (cVertex cg)))
          groups

      assignNames idx cg =
        let v = cVertex cg
            newV = v {vName = renameVertexId treeType idx (vName v)}
         in (idx + 1, cg {cVertex = newV})

      renamedGroups =
        case treeType of
          SupportTree -> sortedGroups
          _ -> snd $ mapAccumL assignNames 0 sortedGroups
   in renamedGroups

updateVerticesInNode
  :: NP.NodePath -> VertexForest -> NE.NonEmpty Node -> Node -> Node
updateVerticesInNode (NP.NodePath Empty) g globals (Array _) =
  Array (V.fromList (NE.toList globals) <> vertexForestToNodeVector g)
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

transform :: Node -> Either Text Node
transform topNode =
  getVertexForest verticesQuery topNode
    >>= getNamesAndUpdateTree
  where
    getNamesAndUpdateTree (globals, vertexForest) =
      let vertexNames = getVertexNamesInForest vertexForest
       in moveVerticesInVertexForest vertexForest
            >>= getUpdatedNamesAndUpdateGlobally globals vertexNames
    getUpdatedNamesAndUpdateGlobally globals oldVertexNames updatedVertexForest =
      let updatedVertexNames = getVertexNamesInForest updatedVertexForest
          updateMap = M.fromList $ on zip M.elems oldVertexNames updatedVertexNames
       in Right . findAndUpdateTextInNode updateMap newCursor $
            updateVerticesInNode verticesQuery updatedVertexForest globals topNode
