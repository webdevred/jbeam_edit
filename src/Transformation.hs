module Transformation (
  transform,
) where

import Core.Node
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (partition, sortOn)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Vector (Vector, (!), (!?), (//))
import GHC.IsList (fromList)

import Core.NodePath qualified as NP
import Data.Foldable qualified as F (foldr, maximumBy)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V

data VertexTreeType
  = LeftTree
  | MiddleTree
  | RightTree
  | SupportTree
  deriving (Eq, Ord, Show)

data VertexTreeEntry
  = VertexEntry Vertex
  | CommentEntry Node
  | MetaEntry Node
  | HeaderEntry Node
  deriving (Show)

data VertexTree = VertexTree
  { tNodes :: NonEmpty VertexTreeEntry
  , tRest :: Maybe VertexTree
  , tType :: VertexTreeType
  }
  deriving (Show)

data Vertex = Vertex
  { vName :: Text
  , vX :: Scientific
  , vY :: Scientific
  , vZ :: Scientific
  }
  deriving (Show)

data CommentGroup = CommentGroup
  { cComments :: [VertexTreeEntry]
  , cVertex :: Vertex
  }
  deriving (Show)

newVertice :: Node -> Maybe Vertex
newVertice (Array ns) = f (V.toList ns)
  where
    f [String name, Number x, Number y, Number z] =
      Just (Vertex {vName = name, vX = x, vY = y, vZ = z})
    f _ = Nothing
newVertice _ = Nothing

isVertice :: Node -> Bool
isVertice node = isJust (newVertice node)

isNonVertice :: Node -> Bool
isNonVertice node = isNothing (newVertice node)

hasVerticePrefix :: Text -> Node -> Bool
hasVerticePrefix verticePrefix node =
  let dropIndex = T.dropWhileEnd isDigit
      verticeName = dropIndex . vName <$> newVertice node
   in verticeName == Just (dropIndex verticePrefix)

getFirstVerticeName :: [Node] -> Maybe Text
getFirstVerticeName (node : _) = vName <$> newVertice node
getFirstVerticeName _ = Nothing

breakVertices :: Maybe Text -> [Node] -> ([Node], [Node])
breakVertices Nothing = error "expected at least one Vertex"
breakVertices (Just verticePrefix) = go []
  where
    go acc [] = (reverse acc, [])
    go acc (node : rest)
      | isNonVertice node = go (node : acc) rest
      | hasVerticePrefix verticePrefix node = go (node : acc) rest
      | isVertice node =
          let (metaBefore, currentTree) = span isNonVertice acc
           in if null currentTree
                then ([node], reverse metaBefore ++ rest)
                else (reverse currentTree, reverse metaBefore ++ (node : rest))
      | otherwise = go (node : acc) rest

toVertexTreeEntry :: Node -> VertexTreeEntry
toVertexTreeEntry node =
  case newVertice node of
    Just vertice -> VertexEntry vertice
    Nothing
      | isObjectNode node -> MetaEntry node
      | otherwise -> HeaderEntry node

mostCommon :: NonEmpty VertexTreeType -> VertexTreeType
mostCommon = NE.head . F.maximumBy (compare `on` length) . NE.group1 . NE.sort

nodesListToTree :: NonEmpty Node -> VertexTree
nodesListToTree nodes =
  let (nonVertices, rest) = NE.span isNonVertice nodes
      verticePrefix = T.dropWhileEnd isDigit <$> getFirstVerticeName rest
      (vertexNodes, rest') = breakVertices verticePrefix rest
      vertices = mapMaybe newVertice vertexNodes
   in case NE.nonEmpty vertices of
        Nothing -> error "expected at least one Vertex"
        Just vs ->
          VertexTree
            { tNodes = NE.fromList (map toVertexTreeEntry (nonVertices ++ vertexNodes))
            , tRest = nodesListToTree <$> NE.nonEmpty rest'
            , tType = mostCommon $ NE.map (determineGroup . vX) vs
            }

getVertexTree :: NP.NodePath -> Node -> VertexTree
getVertexTree np topNode =
  case NP.queryNodes np topNode of
    Just node -> f node
    Nothing -> error ("could not find vertices at path " ++ show verticeQuery)
  where
    f node =
      case node of
        Array ns
          | null ns -> error $ show node
          | otherwise -> nodesListToTree . NE.fromList . V.toList $ ns
        bad -> error $ show bad

vertexInCorrectTree :: VertexTreeType -> Vertex -> Bool
vertexInCorrectTree ttype vertex =
  ttype == determineGroup (vX vertex)

determineGroup :: Scientific -> VertexTreeType
determineGroup x
  | x < -0.09 = RightTree
  | x < 0.09 = MiddleTree
  | otherwise = LeftTree

filterVerticesToMove :: VertexTree -> ([Vertex], Maybe VertexTree)
filterVerticesToMove (VertexTree entries maybeRest ttype) =
  let (removedHere, keptHere) = F.foldr step ([], []) (toList entries)
      step entry (remAcc, keepAcc) = case entry of
        VertexEntry v ->
          if vertexInCorrectTree ttype v
            then (remAcc, entry : keepAcc)
            else (v : remAcc, keepAcc)
        _ -> (remAcc, entry : keepAcc)
      (removedRest, newRest) = case maybeRest of
        Nothing -> ([], Nothing)
        Just subTree ->
          let (rs, newSub) = filterVerticesToMove subTree
           in (rs, newSub)
      allRemoved = removedHere ++ removedRest
   in case NE.nonEmpty keptHere of
        Nothing -> (allRemoved, newRest)
        Just kept -> (allRemoved, Just (VertexTree kept newRest ttype))

moveVertices :: [Vertex] -> VertexTree -> VertexTree
moveVertices vsToMove (VertexTree nodes restTree ttype) =
  let sameTreeType vertex = determineGroup (vX vertex) == ttype
      (toThisGroup, toOtherGroup) = partition sameTreeType vsToMove
      vertexEntries = map VertexEntry toThisGroup
   in VertexTree
        { tNodes = nodes `NE.appendList` vertexEntries
        , tRest = moveVertices toOtherGroup <$> restTree
        , tType = ttype
        }

updateVertices :: VertexTree -> VertexTree
updateVertices vertexTree =
  let (vsToMove, vertexTree') = filterVerticesToMove vertexTree
   in sortVertices . moveVertices vsToMove $ fromJust vertexTree'

entryIsNonVertice :: VertexTreeEntry -> Bool
entryIsNonVertice (VertexEntry _) = False
entryIsNonVertice _ = True

groupVertexWithLeadingComments :: [VertexTreeEntry] -> [CommentGroup]
groupVertexWithLeadingComments = go []
  where
    go _ [] = []
    go acc (entry : rest) =
      case entry of
        CommentEntry _ -> go (acc ++ [entry]) rest
        VertexEntry v -> CommentGroup {cComments = acc, cVertex = v} : go [] rest
        _ -> go [] rest

sortCommentGroups :: [CommentGroup] -> [CommentGroup]
sortCommentGroups = sortOn (\cg -> (vZ (cVertex cg), vY (cVertex cg)))

ungroupCommentGroups :: [CommentGroup] -> [VertexTreeEntry]
ungroupCommentGroups = concatMap (\cg -> cComments cg ++ [VertexEntry (cVertex cg)])

sortVertices :: VertexTree -> VertexTree
sortVertices (VertexTree nodes maybeRest ttype) =
  let (metaNodes, vertexAndComments) = NE.span entryIsNonVertice nodes
      commentGroups = groupVertexWithLeadingComments vertexAndComments
      sortedGroups = sortCommentGroups commentGroups
      sortedEntries = ungroupCommentGroups sortedGroups
   in VertexTree
        { tNodes = NE.fromList (metaNodes <> sortedEntries)
        , tRest = sortVertices <$> maybeRest
        , tType = ttype
        }

verticeQuery :: NP.NodePath
verticeQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "nodes"]

possiblyVertice :: VertexTreeEntry -> Maybe Vertex
possiblyVertice (VertexEntry v) = Just v
possiblyVertice _ = Nothing

getVertexNamesInTree
  :: VertexTree
  -> M.Map (Scientific, Scientific, Scientific) Text
getVertexNamesInTree vertexTree@(VertexTree {tNodes = vs}) =
  let verticeCordNamePair vertice = ((vX vertice, vY vertice, vZ vertice), vName vertice)
      getVertexNames =
        M.fromList . mapMaybe (fmap verticeCordNamePair . possiblyVertice) . NE.toList
      restNames =
        case vertexTree of
          VertexTree {tRest = Just r} -> getVertexNamesInTree r
          VertexTree {tRest = Nothing} -> M.empty
   in M.union (getVertexNames vs) restNames

isObjectKeyEqual :: NP.NodeSelector -> Node -> Bool
isObjectKeyEqual (NP.ObjectKey a) (ObjectKey (String b, _)) = a == b
isObjectKeyEqual _ _ = False

vertexTreeToNodeVector :: VertexTree -> Vector Node
vertexTreeToNodeVector (VertexTree {tNodes = nodes, tRest = maybeOtherTree}) =
  let currentNodes = V.fromList . NE.toList . NE.map vertexEntryToNode $ nodes
      otherNodes = maybe V.empty vertexTreeToNodeVector maybeOtherTree
      vertexEntryToNode entry =
        case entry of
          (CommentEntry node) -> node
          (HeaderEntry node) -> node
          (MetaEntry node) -> node
          (VertexEntry vertex) ->
            let name = String . vName $ vertex
                x = Number . vX $ vertex
                y = Number . vY $ vertex
                z = Number . vZ $ vertex
             in Array . fromList $ [name, x, y, z]
   in currentNodes <> otherNodes

updateVerticesInNode :: NP.NodePath -> VertexTree -> Node -> Node
updateVerticesInNode (NP.NodePath Empty) g (Array _) = Array (vertexTreeToNodeVector g)
updateVerticesInNode (NP.NodePath ((NP.ArrayIndex i) :<| qrest)) g (Array children) =
  let updateInNode nodeToUpdate =
        children // [(i, updateVerticesInNode (NP.NodePath qrest) g nodeToUpdate)]
   in Array $ maybe children updateInNode (children !? i)
updateVerticesInNode (NP.NodePath ((NP.ObjectIndex i) :<| qrest)) g (Object children) =
  let updateInNode _ =
        children // [(i, updateVerticesInNode (NP.NodePath qrest) g (children ! i))]
   in Object $ maybe children updateInNode (children !? i)
updateVerticesInNode (NP.NodePath (k@(NP.ObjectKey _) :<| qrest)) g (Object children) =
  let updateInNode i =
        children // [(i, updateVerticesInNode (NP.NodePath qrest) g (children ! i))]
   in Object . maybe children updateInNode $
        V.findIndex (isObjectKeyEqual k) children
updateVerticesInNode query g (ObjectKey (k, v)) = ObjectKey (k, updateVerticesInNode query g v)
updateVerticesInNode _ _ a = a

transform :: Node -> Node
transform topNode =
  let vertexTree = getVertexTree verticeQuery topNode
      vertexNames = getVertexNamesInTree vertexTree
      updatedVertexTree = updateVertices vertexTree
      updatedVertexNames = getVertexNamesInTree updatedVertexTree
   in error $ show updatedVertexTree
