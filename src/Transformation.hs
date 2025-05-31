module Transformation (
  transform,
) where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Core.Node (Node (..), isCommentNode)
import Data.Char (isDigit)
import Data.Foldable1 (maximumBy)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.Map (Map)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Vector (Vector, (!), (!?), (//))
import GHC.IsList (fromList)

import Core.NodeCursor qualified as NC
import Core.NodePath qualified as NP
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V

data VertexGroupType
  = LeftGroup
  | MiddleGroup
  | RightGroup
  deriving (Eq, Ord, Show)

type VertexIndex = Int

type VertexGroupMap = Map VertexGroupType VertexGroup

type UpdateMap = Map (Scientific, Scientific, Scientific) Text

data VertexBlock = VertexBlock
  { bPreNodes :: [Node]
  , bVertices :: Maybe (NonEmpty Vertex)
  }
  deriving (Show)

data VertexGroup = VertexGroup
  { gName :: Text
  , gStartIndex :: VertexIndex
  , gSize :: VertexIndex
  , gVertices :: Maybe (NonEmpty Vertex)
  , gFresh :: Bool
  }
  deriving (Show)

data Vertex = Vertex
  { vName :: Text
  , vX :: Scientific
  , vY :: Scientific
  , vZ :: Scientific
  }
  deriving (Show)

verticeQuery :: NP.NodePath
verticeQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "nodes"]

dropIndex :: Text -> Text
dropIndex = T.dropWhileEnd isDigit

groupName :: Node -> Maybe Text
groupName n =
  case NP.select (NP.ArrayIndex 0) n of
    Just (String s) -> Just $ dropIndex s
    _ -> Nothing

newVertex :: Node -> Maybe Vertex
newVertex (Array n) = f . V.toList $ n
  where
    f [String name, Number x, Number y, Number z] =
      guard (isDigit $ T.last name)
        >> Just (Vertex {vName = name, vX = x, vY = y, vZ = z})
    f _ = Nothing
newVertex _ = Nothing

determineGroup :: Scientific -> VertexGroupType
determineGroup x
  | x < -0.09 = RightGroup
  | x < 0.09 = MiddleGroup
  | otherwise = LeftGroup

mostCommon :: NonEmpty VertexGroupType -> VertexGroupType
mostCommon = NE.head . maximumBy (compare `on` length) . NE.group1 . NE.sort

setGroupAcc :: VertexGroup -> VertexGroupMap -> VertexGroupMap
setGroupAcc g acc =
  maybe acc (\vs -> M.insert (typeForVerticeList vs) g acc) (gVertices g)
  where
    typeForVerticeList = mostCommon . NE.map (determineGroup . vX)

newVertexGroup :: VertexIndex -> Text -> Vertex -> VertexGroup
newVertexGroup i name vertice =
  VertexGroup
    { gFresh = False
    , gStartIndex = i
    , gSize = 1
    , gName = dropIndex name
    , gVertices = Just $ NE.singleton vertice
    }

nodeBelongsToGroup :: VertexGroup -> Node -> Bool
nodeBelongsToGroup (VertexGroup {gName = name}) n = Just name == groupName n

validateNodeVertices
  :: ([VertexGroupType], Maybe (NonEmpty Scientific, Text)) -> [Node] -> Bool
validateNodeVertices state (n : rest) =
  case state of
    (groups, Nothing)
      | isVertex ->
          validateNodeVertices
            (groups, Just (NE.singleton xCord, groupName'))
            rest
      | otherwise -> validateNodeVertices (groups, Nothing) rest
    (groups, Just (xs, currentGroupName))
      | isNeitherVertexNorComment && typeForXCordList xs `elem` groups -> False
      | isNeitherVertexNorComment ->
          validateNodeVertices (typeForXCordList xs : groups, Nothing) rest
      | isCommentNode n ->
          validateNodeVertices (groups, Just (xs, currentGroupName)) rest
      | Just currentGroupName == groupName n ->
          validateNodeVertices (groups, Just (xCord <| xs, currentGroupName)) rest
      | otherwise -> False
  where
    typeForXCordList = mostCommon . NE.map determineGroup
    xCord = vX (fromJust vertex)
    groupName' = dropIndex . vName $ fromJust vertex
    vertex = newVertex n
    isVertex = isJust vertex
    isNeitherVertexNorComment = isNothing vertex && not (isCommentNode n)
validateNodeVertices _ [] = True

nodeToVertexGroupList :: [VertexGroup] -> Int -> Node -> [VertexGroup]
nodeToVertexGroupList acc i n =
  case newVertex n of
    Just vertice ->
      case acc of
        [] -> [newVertexGroup i (vName vertice) vertice]
        (first : accRest) ->
          if nodeBelongsToGroup first n
            then
              first
                { gSize = i - gStartIndex first
                , gVertices = (vertice <|) <$> gVertices first
                }
                : accRest
            else newVertexGroup i (vName vertice) vertice : acc
    Nothing -> acc

getVertexGroups :: NP.NodePath -> Node -> VertexGroupMap
getVertexGroups q n =
  case NP.queryNodes q n of
    Just (Array n')
      | validateNodeVertices ([], Nothing) (V.toList n') ->
          foldr setGroupAcc M.empty . V.ifoldl' nodeToVertexGroupList [] $ n'
      | otherwise ->
          error
            "the nodes needs to restructured but restructuring is not implemented"
    _ -> error "cannot find node with vertices"

isObjectKeyEqual :: NP.NodeSelector -> Node -> Bool
isObjectKeyEqual (NP.ObjectKey a) (ObjectKey (String b, _)) = a == b
isObjectKeyEqual _ _ = False

findAndUpdateTextInNode :: Map Text Text -> NC.NodeCursor -> Node -> Node
findAndUpdateTextInNode m cursor node =
  case node of
    Array arr
      | NC.comparePathAndCursor verticeQuery cursor -> Array arr
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

newGroupIndex
  :: [(VertexGroupType, VertexGroup)] -> VertexGroupType -> VertexIndex
newGroupIndex groups newGroupType =
  case newGroupType of
    LeftGroup ->
      case groups of
        ((_, g) : _) -> gStartIndex g - 1
        _ -> error "expected to find MiddleGroup or RightGroup in groups"
    RightGroup ->
      case reverse groups of
        ((_, g) : _) -> gStartIndex g + gSize g + 1
        _ -> error "expected to find LeftGroup or MiddleGroup in groups"
    MiddleGroup ->
      case groups of
        ((LeftGroup, g) : _) -> gStartIndex g + gSize g + 1
        [(RightGroup, g)] -> gStartIndex g - 1
        [_, (RightGroup, g)] -> gStartIndex g - 1
        _ -> error "expected to find LeftGroup or RightGroup in groups"

groupTypeToChar :: VertexGroupType -> Text
groupTypeToChar LeftGroup = "l"
groupTypeToChar MiddleGroup = "m"
groupTypeToChar RightGroup = "r"

newGroupName :: [(VertexGroupType, VertexGroup)] -> VertexGroupType -> Text
newGroupName [(_, g)] groupType = gName g <> groupTypeToChar groupType
newGroupName ((_, g1) : (_, g2) : _) groupType =
  case on T.commonPrefixes gName g1 g2 of
    Just (prefix, _, _) -> prefix <> groupTypeToChar groupType
    _ -> gName g1 <> groupTypeToChar groupType
newGroupName [] groupType = groupTypeToChar groupType

addVertex :: Vertex -> Maybe (NonEmpty Vertex) -> Maybe (NonEmpty Vertex)
addVertex vertice Nothing = Just $ NE.singleton vertice
addVertex vertice (Just vs) = Just $ vertice <| vs

moveVertexToGroup
  :: VertexGroupType -> Vertex -> VertexGroupMap -> VertexGroupMap
moveVertexToGroup gType vert gs
  | gType == destGroup = gs
  | M.member destGroup gs = M.update addVertexToGroup destGroup gs
  | otherwise =
      let currentGroupList = M.toList gs
          i = newGroupIndex currentGroupList destGroup
          name = newGroupName currentGroupList destGroup
          group = newVertexGroup i name vert
       in M.insert destGroup group {gFresh = True, gSize = 0} gs
  where
    destGroup = determineGroup . vX $ vert
    addVertexToGroup g = Just g {gVertices = addVertex vert $ gVertices g}

rejectVertices :: VertexGroupType -> VertexGroup -> VertexGroup
rejectVertices t g =
  g
    { gVertices =
        gVertices g >>= NE.nonEmpty . NE.filter ((==) t . determineGroup . vX)
    }

moveVertices
  :: VertexGroupType -> VertexGroup -> VertexGroupMap -> VertexGroupMap
moveVertices gType g gs =
  M.mapWithKey rejectVertices
    . foldr (moveVertexToGroup gType) gs
    . maybe [] NE.toList
    $ gVertices g

updateVertexName :: Text -> (VertexIndex, Vertex) -> Vertex
updateVertexName name (index, vertice) =
  vertice {vName = name <> T.pack (show index)}

updateVertexNames :: VertexGroup -> VertexGroup
updateVertexNames g =
  let vertices = NE.sortBy (on compare $ vZ &&& vY) <$> gVertices g
      indexVertices = NE.zip (NE.fromList [0 ..])
      updatedVertices =
        NE.map (updateVertexName $ gName g) . indexVertices <$> vertices
   in g {gVertices = updatedVertices}

updateVerticesInGroup :: VertexGroupMap -> VertexGroupMap
updateVerticesInGroup gs =
  M.map updateVertexNames $ M.foldrWithKey moveVertices gs gs

verticeToNode :: Vertex -> Node
verticeToNode (Vertex {vName = name, vX = x, vY = y, vZ = z}) =
  Array $ V.fromList [String name, Number x, Number y, Number z]

newGroupHeader :: VertexGroup -> Vector Node
newGroupHeader (VertexGroup {gFresh = True}) =
  V.fromList [SinglelineComment "ny grupp", Object objKey]
  where
    objKey = V.singleton $ ObjectKey (String "group", String "new_group")
newGroupHeader _ = V.empty

succIfNonZero :: Int -> Int
succIfNonZero 0 = 0
succIfNonZero i = i + 1

updateNode :: NP.NodePath -> VertexGroup -> Node -> Node
updateNode (NP.NodePath Empty) g (Array a) =
  let vertices = gVertices g
      startIndex =
        fromMaybe (gStartIndex g) $ V.findIndex (nodeBelongsToGroup g) a
      endIndex = startIndex + succIfNonZero (gSize g)
      beginNodes = V.slice 0 startIndex a
      groupHeader = newGroupHeader g
      verticeNodes =
        V.fromList (maybe [] (map verticeToNode . NE.toList) vertices)
      endNodes = V.slice endIndex (V.length a - endIndex) a
   in Array $ V.concat [beginNodes, groupHeader, verticeNodes, endNodes]
updateNode (NP.NodePath ((NP.ArrayIndex i) :<| qrest)) g (Array children) =
  let updateInNode nodeToUpdate =
        children // [(i, updateNode (NP.NodePath qrest) g nodeToUpdate)]
   in Array $ maybe children updateInNode (children !? i)
updateNode (NP.NodePath ((NP.ObjectIndex i) :<| qrest)) g (Object children) =
  let updateInNode _ =
        children // [(i, updateNode (NP.NodePath qrest) g (children ! i))]
   in Object $ maybe children updateInNode (children !? i)
updateNode (NP.NodePath (k@(NP.ObjectKey _) :<| qrest)) g (Object children) =
  let updateInNode i =
        children // [(i, updateNode (NP.NodePath qrest) g (children ! i))]
   in Object . maybe children updateInNode $
        V.findIndex (isObjectKeyEqual k) children
updateNode query g (ObjectKey (k, v)) = ObjectKey (k, updateNode query g v)
updateNode _ _ a = a

verticeNameMap :: VertexGroup -> UpdateMap -> UpdateMap
verticeNameMap g acc =
  let sortKeys = map (\v -> ((vX v, vY v, vZ v), vName v)) . NE.toList
      vertices = gVertices g
   in M.union acc . M.fromList $ maybe [] sortKeys vertices

transform :: Node -> Node
transform ns =
  let verticeGroups = getVertexGroups verticeQuery ns
      verticeNames = M.foldr verticeNameMap M.empty verticeGroups
      updatedGroups = updateVerticesInGroup verticeGroups
      updatedVertexNames = M.foldr verticeNameMap M.empty updatedGroups
      updateMap = M.fromList $ on zip M.elems verticeNames updatedVertexNames
   in findAndUpdateTextInNode updateMap NC.newCursor $
        foldr (updateNode verticeQuery) ns updatedGroups
