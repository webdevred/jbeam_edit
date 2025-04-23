module Transformation
  ( transform
  ) where

import Control.Arrow ((&&&))
import Control.Monad ((>=>), guard)
import Data.Char (isDigit)
import Data.Foldable1 (maximumBy)
import Data.Function (on)
import Data.List qualified as L (foldl')
import Data.List.NonEmpty qualified as LV
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.Map qualified as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector, (!), (!?), (//))
import Data.Vector qualified as V
import GHC.IsList (fromList)

import NodePath qualified as NP
import Parsing (Node(..))

data VerticeGroupType
  = LeftGroup
  | MiddleGroup
  | RightGroup
  deriving (Eq, Ord, Show)

type VerticeIndex = Int

type VerticeGroupMap = Map VerticeGroupType VerticeGroup

type UpdateMap = Map (Scientific, Scientific, Scientific) Text

data VerticeGroup = VerticeGroup
  { gName :: Text
  , gStartIndex :: VerticeIndex
  , gSize :: VerticeIndex
  , gVertices :: Maybe (NonEmpty Vertice)
  , gFresh :: Bool
  } deriving (Show)

data Vertice = Vertice
  { vName :: Text
  , vX :: Scientific
  , vY :: Scientific
  , vZ :: Scientific
  } deriving (Show)

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
queryNodes (NP.NodePath s) = L.foldl' ((>=>)) id (map select s) . Just

dropIndex :: Text -> Text
dropIndex = T.dropWhileEnd isDigit

groupName :: Node -> Maybe Text
groupName n =
  case select (NP.ArrayIndex 0) n of
    Just (String s) -> Just $ dropIndex s
    _ -> Nothing

newVertice :: Node -> Maybe Vertice
newVertice (Array n) = f . V.toList $ n
  where
    f [String name, Number x, Number y, Number z] =
      guard (isDigit $ T.last name)
        >> Just (Vertice {vName = name, vX = x, vY = y, vZ = z})
    f _ = Nothing
newVertice _ = Nothing

determineGroup :: Scientific -> VerticeGroupType
determineGroup x
  | x < -0.09 = RightGroup
  | x < 0.09 = MiddleGroup
  | otherwise = LeftGroup

setGroupAcc :: VerticeGroup -> VerticeGroupMap -> VerticeGroupMap
setGroupAcc g acc = maybe acc (\vs -> M.insert (gType vs) g acc) (gVertices g)
  where
    gType = mostCommon . LV.map (determineGroup . vX)
    mostCommon = LV.head . maximumBy (compare `on` length) . LV.group1 . LV.sort

newVerticeGroup :: VerticeIndex -> Text -> Vertice -> VerticeGroup
newVerticeGroup i name vertice =
  VerticeGroup
    { gFresh = False
    , gStartIndex = i
    , gSize = 1
    , gName = dropIndex name
    , gVertices = Just $ LV.singleton vertice
    }

nodeBelongsToGroup :: VerticeGroup -> Node -> Bool
nodeBelongsToGroup (VerticeGroup {gName = name}) n = Just name == groupName n

nodeToVerticeGroupList :: [VerticeGroup] -> Int -> Node -> [VerticeGroup]
nodeToVerticeGroupList acc i n =
  case newVertice n of
    Just vertice ->
      case acc of
        [] -> [newVerticeGroup i (vName vertice) vertice]
        (first:accRest) ->
          if nodeBelongsToGroup first n
            then first
                   { gSize = i - gStartIndex first
                   , gVertices = (vertice <|) <$> gVertices first
                   }
                   : accRest
            else newVerticeGroup i (vName vertice) vertice : acc
    Nothing -> acc

getVerticeGroups :: NP.NodePath -> Node -> VerticeGroupMap
getVerticeGroups q n =
  case queryNodes q n of
    Just (Array n') ->
      foldr setGroupAcc M.empty . V.ifoldl' nodeToVerticeGroupList [] $ n'
    _ -> error "cannot find node with vertices"

isObjectKeyEqual :: NP.NodeSelector -> Node -> Bool
isObjectKeyEqual (NP.ObjectKey a) (ObjectKey (String b, _)) = a == b
isObjectKeyEqual _ _ = False

findAndUpdateTextInNode :: Map Text Text -> Node -> Node
findAndUpdateTextInNode m (Array a) =
  Array $ V.map (findAndUpdateTextInNode m) a
findAndUpdateTextInNode m (Object a) =
  Object $ V.map (findAndUpdateTextInNode m) a
findAndUpdateTextInNode m (ObjectKey (k, v)) =
  case k of
    String "nodes" -> ObjectKey (k, v)
    _ -> ObjectKey (k, findAndUpdateTextInNode m v)
findAndUpdateTextInNode m (String s) =
  case M.lookup s m of
    Just s' -> String s'
    Nothing -> String s
findAndUpdateTextInNode _ a = a

newGroupIndex ::
     [(VerticeGroupType, VerticeGroup)] -> VerticeGroupType -> VerticeIndex
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

groupTypeToChar :: VerticeGroupType -> Text
groupTypeToChar LeftGroup = "l"
groupTypeToChar MiddleGroup = "m"
groupTypeToChar RightGroup = "r"

newGroupName :: [(VerticeGroupType, VerticeGroup)] -> VerticeGroupType -> Text
newGroupName [(_, g)] groupType = gName g <> groupTypeToChar groupType
newGroupName ((_, g1):(_, g2):_) groupType =
  case on T.commonPrefixes gName g1 g2 of
    Just (prefix, _, _) -> prefix <> groupTypeToChar groupType
    _ -> gName g1 <> groupTypeToChar groupType
newGroupName [] groupType = groupTypeToChar groupType

addVertice :: Vertice -> Maybe (NonEmpty Vertice) -> Maybe (NonEmpty Vertice)
addVertice vertice Nothing = Just $ LV.singleton vertice
addVertice vertice (Just vs) = Just $ vertice <| vs

moveVerticeToGroup ::
     VerticeGroupType -> Vertice -> VerticeGroupMap -> VerticeGroupMap
moveVerticeToGroup gType vert gs
  | gType == destGroup = gs
  | M.member destGroup gs = M.update addVerticeToGroup destGroup gs
  | otherwise =
    let currentGroupList = M.toList gs
        i = newGroupIndex currentGroupList destGroup
        name = newGroupName currentGroupList destGroup
        group = newVerticeGroup i name vert
     in M.insert destGroup group {gFresh = True, gSize = 0} gs
  where
    destGroup = determineGroup . vX $ vert
    addVerticeToGroup g = Just g {gVertices = addVertice vert $ gVertices g}

rejectVertices :: VerticeGroupType -> VerticeGroup -> VerticeGroup
rejectVertices t g =
  g
    { gVertices =
        gVertices g >>= LV.nonEmpty . LV.filter ((==) t . determineGroup . vX)
    }

moveVertices ::
     VerticeGroupType -> VerticeGroup -> VerticeGroupMap -> VerticeGroupMap
moveVertices gType g gs =
  M.mapWithKey rejectVertices
    . foldr (moveVerticeToGroup gType) gs
    . maybe [] LV.toList
    $ gVertices g

updateVerticeName :: Text -> (VerticeIndex, Vertice) -> Vertice
updateVerticeName name (index, vertice) =
  vertice {vName = name <> T.pack (show index)}

updateVerticeNames :: VerticeGroup -> VerticeGroup
updateVerticeNames g =
  let vertices = LV.sortBy (on compare $ vZ &&& vY) <$> gVertices g
      indexVertices = LV.zip (LV.fromList [0 ..])
      updatedVertices =
        LV.map (updateVerticeName $ gName g) . indexVertices <$> vertices
   in g {gVertices = updatedVertices}

updateVerticesInGroup :: VerticeGroupMap -> VerticeGroupMap
updateVerticesInGroup gs =
  M.map updateVerticeNames $ M.foldrWithKey moveVertices gs gs

verticeToNode :: Vertice -> Node
verticeToNode (Vertice {vName = name, vX = x, vY = y, vZ = z}) =
  Array $ V.fromList [String name, Number x, Number y, Number z]

newGroupHeader :: VerticeGroup -> Vector Node
newGroupHeader (VerticeGroup {gFresh = True}) =
  V.fromList [SinglelineComment "ny grupp", Object objKey]
  where
    objKey = V.singleton $ ObjectKey (String "group", String "new_group")
newGroupHeader _ = V.empty

succIfNonZero :: Int -> Int
succIfNonZero 0 = 0
succIfNonZero i = i + 1

updateNode :: NP.NodePath -> VerticeGroup -> Node -> Node
updateNode (NP.NodePath []) n (Array a) =
  let vertices = gVertices n
      startIndex =
        fromMaybe (gStartIndex n) $ V.findIndex (nodeBelongsToGroup n) a
      endIndex = startIndex + succIfNonZero (gSize n)
      beginNodes = V.slice 0 startIndex a
      groupHeader = newGroupHeader n
      verticeNodes =
        V.fromList (maybe [] (map verticeToNode . LV.toList) vertices)
      endNodes = V.slice endIndex (V.length a - endIndex) a
   in Array $ V.concat [beginNodes, groupHeader, verticeNodes, endNodes]
updateNode (NP.NodePath ((NP.ArrayIndex i):qrest)) n (Array a) =
  case a !? i of
    Just a' -> Array $ a // [(i, updateNode (NP.NodePath qrest) n a')]
    Nothing -> Array a
updateNode (NP.NodePath ((NP.ObjectIndex i):qrest)) n (Object a) =
  case a !? i of
    Just _ -> Object $ a // [(i, updateNode (NP.NodePath qrest) n (a ! i))]
    Nothing -> Object a
updateNode (NP.NodePath (k@(NP.ObjectKey _):qrest)) n (Object a) =
  case V.findIndex (isObjectKeyEqual k) a of
    Just i -> Object $ a // [(i, updateNode (NP.NodePath qrest) n (a ! i))]
    Nothing -> Object a
updateNode qs n (ObjectKey (k, v)) = ObjectKey (k, updateNode qs n v)
updateNode _ _ a = a

verticeNameMap :: VerticeGroup -> UpdateMap -> UpdateMap
verticeNameMap g acc =
  let sortKeys = map (\v -> ((vX v, vY v, vZ v), vName v)) . LV.toList
      vertices = gVertices g
   in M.union acc . M.fromList $ maybe [] sortKeys vertices

transform :: Node -> Node
transform ns =
  let query = fromList [NP.ObjectIndex 0, NP.ObjectKey "nodes"]
      verticeGroups = getVerticeGroups query ns
      verticeNames = M.foldr verticeNameMap M.empty verticeGroups
      updatedGroups = updateVerticesInGroup verticeGroups
      updatedVerticeNames = M.foldr verticeNameMap M.empty updatedGroups
      updateMap = M.fromList $ on zip M.elems verticeNames updatedVerticeNames
   in findAndUpdateTextInNode updateMap
        $ foldr (updateNode query) ns updatedGroups
