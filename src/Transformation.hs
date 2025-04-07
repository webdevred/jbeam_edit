module Transformation
  ( transform
  ) where

import Control.Monad
import Data.Char (isDigit)
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as V
import Parsing (Node(..))

import Control.Arrow ((&&&))

data NodeQuery
  = Index Int
  | Key Text
  | NumericKey Int
  deriving (Show)

data VerticeGroupType
  = LeftGroup
  | MiddleGroup
  | RightGroup
  deriving (Eq, Ord, Show)

type VerticeIndex = Int

type VerticeGroupMap = Map VerticeGroupType VerticeGroup

data VerticeGroup =
  VerticeGroup
    { gUpdateMap :: Map Text Text
    , gName :: Text
    , gStartIndex :: VerticeIndex
    , gSize :: VerticeIndex
    , gVertices :: Vector Vertice
    , gFresh :: Bool
    }
  deriving (Show)

data Vertice =
  Vertice
    { vName :: Text
    , vX :: Scientific
    , vY :: Scientific
    , vZ :: Scientific
    }
  deriving (Show)

extractVal :: Node -> Node
extractVal (ObjectKey (_, val)) = val
extractVal _ = error "unreachable"

select :: NodeQuery -> Node -> Maybe Node
select (Index i) (Array ns) = ns !? i
select (Key k) (Object ns) = extractVal <$> V.find compareKey ns
  where
    compareKey (ObjectKey (String keyText, _)) = keyText == k
    compareKey _ = error "unreachable"
select (NumericKey i) (Object a) = extractVal <$> a !? i
select _ _ = Nothing

queryNodes :: [NodeQuery] -> Node -> Maybe Node
queryNodes qs = L.foldl' (>=>) id (map select qs) . Just

dropIndex :: Text -> Text
dropIndex = T.dropWhileEnd isDigit

groupName :: Node -> Maybe Text
groupName n =
  case select (Index 0) n of
    Just (String s) -> Just $ dropIndex s
    _ -> Nothing

newVertice :: Node -> Maybe Vertice
newVertice (Array n) = f . V.toList $ n
  where
    f [String name, Number x, Number y, Number z] =
      if isDigit (T.last name)
        then Just Vertice {vName = name, vX = x, vY = y, vZ = z}
        else Nothing
    f _ = Nothing
newVertice _ = Nothing

determineGroup :: Scientific -> VerticeGroupType
determineGroup x
  | x < -0.09 = RightGroup
  | x < 0.09 = MiddleGroup
  | otherwise = LeftGroup

setGroupAcc :: VerticeGroup -> VerticeGroupMap -> VerticeGroupMap
setGroupAcc g acc = M.insert gType g acc
  where
    gType = mostCommon . V.toList . V.map (determineGroup . vX) . gVertices $ g
    mostCommon = head . L.maximumBy (compare `on` length) . L.group . L.sort

newVerticeGroup :: VerticeIndex -> Text -> Vertice -> VerticeGroup
newVerticeGroup i name vertice =
  VerticeGroup
    { gFresh = False
    , gStartIndex = i
    , gSize = 1
    , gName = dropIndex name
    , gVertices = V.singleton vertice
    , gUpdateMap = M.empty
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
                   , gVertices = vertice `V.cons` gVertices first
                   } :
                 accRest
            else newVerticeGroup i (vName vertice) vertice : acc
    Nothing -> acc

getVerticeGroups :: [NodeQuery] -> Node -> VerticeGroupMap
getVerticeGroups q n =
  case queryNodes q n of
    Just (Array n') ->
      foldr setGroupAcc M.empty . V.ifoldl' nodeToVerticeGroupList [] $ n'
    _ -> error "cannot find node with vertices"

isObjectKeyEqual :: NodeQuery -> Node -> Bool
isObjectKeyEqual (Key a) (ObjectKey (String b, _)) = a == b
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

findVerticesWithIncorrectGroup ::
     VerticeGroupType
  -> VerticeGroup
  -> ([(VerticeGroupType, Vertice)], VerticeGroupMap)
  -> ([(VerticeGroupType, Vertice)], VerticeGroupMap)
findVerticesWithIncorrectGroup gType g (verticesToMoveAcc, groups) =
  let (verticesToMove, vs) =
        V.foldl' findVerticesToMoveInGroup ([], []) $ gVertices g
   in ( verticesToMove ++ verticesToMoveAcc
      , M.insert gType (g {gVertices = V.fromList vs}) groups)
  where
    findVerticesToMoveInGroup (updateAcc, noUpdateAcc) v =
      let group = determineGroup . vX $ v
       in if group == gType
            then (updateAcc, v : noUpdateAcc)
            else ((group, v) : updateAcc, noUpdateAcc)

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
newGroupName [(_, VerticeGroup {gName = name})] groupType =
  T.append name $ groupTypeToChar groupType
newGroupName [(_, VerticeGroup {gName = name1}), (_, VerticeGroup {gName = name2})] groupType =
  case T.commonPrefixes name1 name2 of
    Just (prefix, _, _) -> T.append prefix $ groupTypeToChar groupType
    _ -> T.append name1 $ groupTypeToChar groupType
newGroupName _ _ = error "unreachable"

addVerticesToGroups ::
     (VerticeGroupType, Vertice) -> VerticeGroupMap -> VerticeGroupMap
addVerticesToGroups (gType, vert) gs =
  if M.member gType gs
    then M.update addVerticeToGroup gType gs
    else let currentGroupList = M.toList gs
             i = newGroupIndex currentGroupList gType
             name = newGroupName currentGroupList gType
          in M.insert
               gType
               (newVerticeGroup i name vert) {gFresh = True, gSize = 0}
               gs
  where
    addVerticeToGroup g = Just g {gVertices = V.cons vert $ gVertices g}

updateVerticeName :: Text -> VerticeIndex -> Vertice -> Vertice
updateVerticeName name index vertice =
  vertice {vName = T.append name $ T.pack $ show index}

updateVerticeNames :: VerticeGroup -> VerticeGroup
updateVerticeNames g =
  let vertices = V.fromList . L.sortOn (vZ &&& vY) . V.toList $ gVertices g
      verticeNames = V.toList . V.map vName $ vertices
      updatedVertices = V.imap (updateVerticeName $ gName g) vertices
      updatedVerticeNames = V.toList . V.map vName $ updatedVertices
      updateMap = M.fromList $ zip verticeNames updatedVerticeNames
   in g {gVertices = updatedVertices, gUpdateMap = updateMap}

updateVerticesInGroup :: VerticeGroupMap -> VerticeGroupMap
updateVerticesInGroup gs =
  let (verticesInNeedOfMove, verticeGroups') =
        M.foldrWithKey findVerticesWithIncorrectGroup ([], M.empty) gs
   in M.map updateVerticeNames $
      foldr addVerticesToGroups verticeGroups' verticesInNeedOfMove

verticeToNode :: Vertice -> Node
verticeToNode (Vertice {vName = name, vX = x, vY = y, vZ = z}) =
  Array $ V.fromList [String name, Number x, Number y, Number z]

newGroupHeader :: VerticeGroup -> Vector Node
newGroupHeader (VerticeGroup {gFresh = True, gName = name}) =
  V.fromList [SinglelineComment name, Object objKey]
  where
    objKey = V.singleton $ ObjectKey (String "group", String name)
newGroupHeader _ = V.empty

succIfNonZero :: Int -> Int
succIfNonZero 0 = 0
succIfNonZero i = i + 1

updateNode :: [NodeQuery] -> VerticeGroup -> Node -> Node
updateNode [] n (Array a) =
  let vertices = gVertices n
      startIndex =
        fromMaybe (gStartIndex n) $ V.findIndex (nodeBelongsToGroup n) a
      endIndex = startIndex + (succIfNonZero $ gSize n)
      beginNodes = V.slice 0 startIndex a
      groupHeader = newGroupHeader n
      verticeNodes = V.map verticeToNode vertices
      endNodes = V.slice endIndex (V.length a - endIndex) a
   in Array $ V.concat [beginNodes, groupHeader, verticeNodes, endNodes]
updateNode ((Index i):qrest) n (Array a) =
  case a !? i of
    Just a' -> Array $ a // [(i, updateNode qrest n a')]
    Nothing -> Array a
updateNode ((NumericKey i):qrest) n (Object a) =
  case a !? i of
    Just _ -> Object $ a // [(i, updateNode qrest n (a ! i))]
    Nothing -> Object a
updateNode (k@(Key _):qrest) n (Object a) =
  case V.findIndex (isObjectKeyEqual k) a of
    Just i -> Object $ a // [(i, updateNode qrest n (a ! i))]
    Nothing -> Object a
updateNode qs n (ObjectKey (k, v)) = ObjectKey (k, updateNode qs n v)
updateNode _ _ a = a

transform :: Node -> Node
transform ns =
  let query = [NumericKey 0, Key "nodes"]
      verticeGroups = getVerticeGroups query ns
      updatedGroups = updateVerticesInGroup verticeGroups
      updateMap = M.unions $ M.map gUpdateMap updatedGroups
   in findAndUpdateTextInNode updateMap $
      foldr (updateNode query) ns updatedGroups
