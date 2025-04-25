module Transformation
  ( transform
  ) where

import Control.Arrow ((&&&))
import Control.Monad ((>=>), guard)
import Data.Bool (bool)
import Data.Char (isDigit)
import Data.Foldable1 (maximumBy)
import Data.Function (on)
import Data.List qualified as L (foldl')
import Data.List.NonEmpty qualified as LV
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.Map qualified as M
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector, (!), (!?), (//))
import Data.Vector qualified as V
import GHC.IsList (fromList)

import NodeCursor qualified as NC
import NodePath qualified as NP
import Parsing (Node(..))

data VertexGroupType
  = LeftGroup
  | MiddleGroup
  | RightGroup
  deriving (Eq, Ord, Show)

type VertexIndex = Int

type VertexGroupMap = Map VertexGroupType VertexGroup

type UpdateMap = Map (Scientific, Scientific, Scientific) Text

data VertexBlock = VertexBlock
  { bProps :: Map Text Node
  , bVertices :: Maybe (NonEmpty Vertex)
  } deriving (Show)

data VertexGroup = VertexGroup
  { gName :: Text
  , gStartIndex :: VertexIndex
  , gSize :: VertexIndex
  , gVertices :: Maybe (NonEmpty Vertex)
  , gFresh :: Bool
  } deriving (Show)

data Vertex = Vertex
  { vName :: Text
  , vX :: Scientific
  , vY :: Scientific
  , vZ :: Scientific
  } deriving (Show)

verticeQuery :: NP.NodePath
verticeQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "nodes"]

extractValInKey :: Node -> Maybe Node
extractValInKey (ObjectKey (_, val)) = Just val
extractValInKey _ = Nothing

select :: NP.NodeSelector -> Node -> Maybe Node
select (NP.ArrayIndex i) (Array ns) = ns !? i
select (NP.ObjectKey k) (Object ns) = extractValInKey =<< V.find compareKey ns
  where
    compareKey (ObjectKey (String keyText, _)) = keyText == k
    compareKey _ = False
select (NP.ObjectIndex i) (Object a) = extractValInKey =<< a !? i
select _ _ = Nothing

queryNodes :: NP.NodePath -> Node -> Maybe Node
queryNodes (NP.NodePath s) = L.foldl' (>=>) id (map select s) . Just

dropIndex :: Text -> Text
dropIndex = T.dropWhileEnd isDigit

groupName :: Node -> Maybe Text
groupName n =
  case select (NP.ArrayIndex 0) n of
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

setGroupAcc :: VertexGroup -> VertexGroupMap -> VertexGroupMap
setGroupAcc g acc = maybe acc (\vs -> M.insert (gType vs) g acc) (gVertices g)
  where
    gType = mostCommon . LV.map (determineGroup . vX)
    mostCommon = LV.head . maximumBy (compare `on` length) . LV.group1 . LV.sort

newVertexGroup :: VertexIndex -> Text -> Vertex -> VertexGroup
newVertexGroup i name vertice =
  VertexGroup
    { gFresh = False
    , gStartIndex = i
    , gSize = 1
    , gName = dropIndex name
    , gVertices = Just $ LV.singleton vertice
    }

nodeBelongsToGroup :: VertexGroup -> Node -> Bool
nodeBelongsToGroup (VertexGroup {gName = name}) n = Just name == groupName n

nodeToVertexGroupList :: [VertexGroup] -> Int -> Node -> [VertexGroup]
nodeToVertexGroupList acc i n =
  case newVertex n of
    Just vertice ->
      case acc of
        [] -> [newVertexGroup i (vName vertice) vertice]
        (first:accRest) ->
          if nodeBelongsToGroup first n
            then first
                   { gSize = i - gStartIndex first
                   , gVertices = (vertice <|) <$> gVertices first
                   }
                   : accRest
            else newVertexGroup i (vName vertice) vertice : acc
    Nothing -> acc

getVertexGroups :: NP.NodePath -> Node -> VertexGroupMap
getVertexGroups q n =
  case queryNodes q n of
    Just (Array n') ->
      foldr setGroupAcc M.empty . V.ifoldl' nodeToVertexGroupList [] $ n'
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
    ObjectKey (String key, value) -> updateObjectKey key value
    String s -> String $ M.findWithDefault s s m
    _ -> node
  where
    applyBreadcrumbAndUpdateText index =
      NC.applyCrumb (NC.ArrayIndex index) cursor (findAndUpdateTextInNode m)
    updateObjectKey key value =
      case cursor of
        NC.NodeCursor ((NC.ArrayIndex b):bs) ->
          let crumb = NC.ObjectIndexAndKey (b, key)
              newCursor' = NC.NodeCursor bs
           in ObjectKey
                ( String key
                , NC.applyCrumb
                    crumb
                    newCursor'
                    (findAndUpdateTextInNode m)
                    value)
        NC.NodeCursor _ -> node

newGroupIndex ::
     [(VertexGroupType, VertexGroup)] -> VertexGroupType -> VertexIndex
newGroupIndex groups newGroupType =
  case newGroupType of
    LeftGroup ->
      case groups of
        ((_, g):_) -> gStartIndex g - 1
        _ -> error "expected to find MiddleGroup or RightGroup in groups"
    RightGroup ->
      case reverse groups of
        ((_, g):_) -> gStartIndex g + gSize g + 1
        _ -> error "expected to find LeftGroup or MiddleGroup in groups"
    MiddleGroup ->
      case groups of
        ((LeftGroup, g):_) -> gStartIndex g + gSize g + 1
        [(RightGroup, g)] -> gStartIndex g - 1
        [_, (RightGroup, g)] -> gStartIndex g - 1
        _ -> error "expected to find LeftGroup or RightGroup in groups"

groupTypeToChar :: VertexGroupType -> Text
groupTypeToChar LeftGroup = "l"
groupTypeToChar MiddleGroup = "m"
groupTypeToChar RightGroup = "r"

newGroupName :: [(VertexGroupType, VertexGroup)] -> VertexGroupType -> Text
newGroupName [(_, g)] groupType = gName g <> groupTypeToChar groupType
newGroupName ((_, g1):(_, g2):_) groupType =
  case on T.commonPrefixes gName g1 g2 of
    Just (prefix, _, _) -> prefix <> groupTypeToChar groupType
    _ -> gName g1 <> groupTypeToChar groupType
newGroupName [] groupType = groupTypeToChar groupType

addVertex :: Vertex -> Maybe (NonEmpty Vertex) -> Maybe (NonEmpty Vertex)
addVertex vertice Nothing = Just $ LV.singleton vertice
addVertex vertice (Just vs) = Just $ vertice <| vs

moveVertexToGroup ::
     VertexGroupType -> Vertex -> VertexGroupMap -> VertexGroupMap
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
        gVertices g >>= LV.nonEmpty . LV.filter ((==) t . determineGroup . vX)
    }

moveVertices ::
     VertexGroupType -> VertexGroup -> VertexGroupMap -> VertexGroupMap
moveVertices gType g gs =
  M.mapWithKey rejectVertices
    . foldr (moveVertexToGroup gType) gs
    . maybe [] LV.toList
    $ gVertices g

updateVertexName :: Text -> (VertexIndex, Vertex) -> Vertex
updateVertexName name (index, vertice) =
  vertice {vName = name <> T.pack (show index)}

updateVertexNames :: VertexGroup -> VertexGroup
updateVertexNames g =
  let vertices = LV.sortBy (on compare $ vZ &&& vY) <$> gVertices g
      indexVertices = LV.zip (LV.fromList [0 ..])
      updatedVertices =
        LV.map (updateVertexName $ gName g) . indexVertices <$> vertices
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
updateNode (NP.NodePath []) g (Array a) =
  let vertices = gVertices g
      startIndex =
        fromMaybe (gStartIndex g) $ V.findIndex (nodeBelongsToGroup g) a
      endIndex = startIndex + succIfNonZero (gSize g)
      beginNodes = V.slice 0 startIndex a
      groupHeader = newGroupHeader g
      verticeNodes =
        V.fromList (maybe [] (map verticeToNode . LV.toList) vertices)
      endNodes = V.slice endIndex (V.length a - endIndex) a
   in Array $ V.concat [beginNodes, groupHeader, verticeNodes, endNodes]
updateNode (NP.NodePath ((NP.ArrayIndex i):qrest)) g (Array children) =
  let updateInNode nodeToUpdate =
        children // [(i, updateNode (NP.NodePath qrest) g nodeToUpdate)]
   in Array $ maybe children updateInNode (children !? i)
updateNode (NP.NodePath ((NP.ObjectIndex i):qrest)) g (Object children) =
  let updateInNode _ =
        children // [(i, updateNode (NP.NodePath qrest) g (children ! i))]
   in Object $ maybe children updateInNode (children !? i)
updateNode (NP.NodePath (k@(NP.ObjectKey _):qrest)) g (Object children) =
  let updateInNode i =
        children // [(i, updateNode (NP.NodePath qrest) g (children ! i))]
   in Object . maybe children updateInNode
        $ V.findIndex (isObjectKeyEqual k) children
updateNode query g (ObjectKey (k, v)) = ObjectKey (k, updateNode query g v)
updateNode _ _ a = a

verticeNameMap :: VertexGroup -> UpdateMap -> UpdateMap
verticeNameMap g acc =
  let sortKeys = map (\v -> ((vX v, vY v, vZ v), vName v)) . LV.toList
      vertices = gVertices g
   in M.union acc . M.fromList $ maybe [] sortKeys vertices

transform :: Node -> Node
transform ns =
  let verticeGroups = getVertexGroups verticeQuery ns
      verticeNames = M.foldr verticeNameMap M.empty verticeGroups
      updatedGroups = updateVerticesInGroup verticeGroups
      updatedVertexNames = M.foldr verticeNameMap M.empty updatedGroups
      updateMap = M.fromList $ on zip M.elems verticeNames updatedVertexNames
   in findAndUpdateTextInNode updateMap NC.newCursor
        $ foldr (updateNode verticeQuery) ns updatedGroups
