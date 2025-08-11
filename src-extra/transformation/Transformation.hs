module Transformation (
  transform,
) where

import Core.Node
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (partition, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe)
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector, (!), (!?), (//))
import GHC.IsList (fromList)

import Core.NodeCursor qualified as NC
import Core.NodePath qualified as NP
import Data.Foldable qualified as F (maximumBy)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable qualified as TR (mapAccumL)
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
  deriving (Eq, Show)

data VertexTree = VertexTree
  { tMetaNodes :: [Node]
  , tVertexNodes :: NonEmpty VertexTreeEntry
  , tRest :: Maybe VertexTree
  , tType :: VertexTreeType
  }
  deriving (Show)

data VertexContext = VertexContext
  { ctxMetaAbove :: [Node]
  , ctxEntriesBefore :: [VertexTreeEntry]
  , ctxEntriesAfter :: [VertexTreeEntry]
  , ctxParentType :: VertexTreeType
  , ctxRestAbove :: Maybe VertexTree
  }
  deriving (Show)

data VertexZipper = VertexZipper
  { zFocus :: VertexTree
  , zPath :: [VertexContext]
  }
  deriving (Show)

data ListZ a = ListZ [a] a [a] deriving (Show)

data Vertex = Vertex
  { vName :: Text
  , vX :: Scientific
  , vY :: Scientific
  , vZ :: Scientific
  , vMeta :: Maybe Object
  }
  deriving (Eq, Show)

data CommentGroup = CommentGroup
  { cComments :: [VertexTreeEntry]
  , cVertex :: Vertex
  }
  deriving (Show)

newVertex :: Node -> Maybe Vertex
newVertex (Array ns) = f (V.toList ns)
  where
    f [String name, Number x, Number y, Number z, Object m] =
      Just (Vertex {vName = name, vX = x, vY = y, vZ = z, vMeta = Just m})
    f [String name, Number x, Number y, Number z] =
      Just (Vertex {vName = name, vX = x, vY = y, vZ = z, vMeta = Nothing})
    f _ = Nothing
newVertex _ = Nothing

isVertex :: Node -> Bool
isVertex node = isJust (newVertex node)

isNonVertex :: Node -> Bool
isNonVertex node = isNothing (newVertex node)

dropIndex :: Text -> Text
dropIndex = T.dropWhileEnd isDigit

hasVertexPrefix :: Text -> Node -> Bool
hasVertexPrefix vertexPrefix node =
  let vertexName = dropIndex . vName <$> newVertex node
   in vertexName == Just (dropIndex vertexPrefix)

getFirstVertexName :: [Node] -> Maybe Text
getFirstVertexName (node : _) = vName <$> newVertex node
getFirstVertexName _ = Nothing

isCollision (Array vertex) vertexNames =
  case getVertexName $ V.uncons vertex of
    Just vertexName ->
      if S.member vertexName vertexNames
        then
          Left $ "multiple vertices named " <> vertexName
        else
          Right (S.insert vertexName vertexNames)
    Nothing -> Right vertexNames
  where
    getVertexName (Just (String string, _)) = Just string
    getVertexName _ = Nothing

breakVertices
  :: Maybe Text -> Set Text -> [Node] -> Either Text (Set Text, [Node], [Node])
breakVertices Nothing _ _ = Left "expected at least one Vertex"
breakVertices (Just vertexPrefix) allVertexNames ns = go [] ns allVertexNames
  where
    go acc [] vertexNames = Right (vertexNames, reverse acc, [])
    go acc (node : rest) vertexNames
      | isNonVertex node = go (node : acc) rest vertexNames
      | hasVertexPrefix vertexPrefix node =
          isCollision node vertexNames >>= go (node : acc) rest
      | isVertex node =
          let (metaBefore, currentTree) = span isNonVertex acc
           in if null currentTree
                then Right (vertexNames, [node], reverse metaBefore ++ rest)
                else
                  Right (vertexNames, reverse currentTree, reverse metaBefore ++ (node : rest))
      | otherwise = go (node : acc) rest vertexNames

toVertexTreeEntry :: Node -> VertexTreeEntry
toVertexTreeEntry node =
  case newVertex node of
    Just vertex -> VertexEntry vertex
    Nothing
      | isObjectNode node -> MetaEntry node
      | otherwise -> CommentEntry node

mostCommon :: NonEmpty VertexTreeType -> VertexTreeType
mostCommon = NE.head . F.maximumBy (compare `on` length) . NE.group1 . NE.sort

nodesListToTree :: Set Text -> NonEmpty Node -> Either Text VertexTree
nodesListToTree vertexNames nodes =
  let (nonVertices, rest) = NE.span isNonVertex nodes
      vertexPrefix = T.dropWhileEnd isDigit <$> getFirstVertexName rest
   in breakVertices vertexPrefix vertexNames rest >>= \(vertexNames', vertexNodes, rest') ->
        go nonVertices vertexNodes vertexNames' rest'
  where
    go nonVertices vertexNodes vertexNames rest =
      let vertices = mapMaybe newVertex vertexNodes
       in case NE.nonEmpty vertices of
            Nothing -> Left "expected at least one Vertex"
            Just vs ->
              let tRestResult = case NE.nonEmpty rest of
                    Nothing -> Right Nothing
                    Just restNe -> fmap Just (nodesListToTree vertexNames restNe)
               in case tRestResult of
                    Left err -> Left err
                    Right maybeSubTree ->
                      Right
                        VertexTree
                          { tMetaNodes = nonVertices
                          , tVertexNodes = NE.fromList (map toVertexTreeEntry vertexNodes)
                          , tRest = maybeSubTree
                          , tType = mostCommon $ NE.map (determineGroup . vX) vs
                          }

getVertexTree :: NP.NodePath -> Node -> Either Text VertexTree
getVertexTree np topNode =
  case NP.queryNodes np topNode of
    Just node -> f node
    Nothing -> Left $ "could not find vertices at path " <> T.pack (show verticesQuery)
  where
    f node =
      case node of
        Array ns
          | null ns -> Left . T.pack . show $ ns
          | otherwise -> nodesListToTree S.empty . NE.fromList . V.toList $ ns
        bad -> Left . T.pack . show $ bad

determineGroup :: Scientific -> VertexTreeType
determineGroup x
  | x < -0.09 = RightTree
  | x < 0.09 = MiddleTree
  | otherwise = LeftTree

metaKey :: Node -> Maybe Node
metaKey (ObjectKey (key, _)) = Just key
metaKey _ = Nothing

getVertexTreeGlobals :: VertexTree -> ([Node], VertexTree)
getVertexTreeGlobals (VertexTree metas vertices restTree ttype) =
  let metaElem meta metas' =
        let maybeKey = metaKey meta
         in case maybeKey of
              Nothing -> False
              _ -> maybeKey `elem` map metaKey metas'
      existsInRestTree meta =
        any
          (\restEntries -> meta `metaElem` subNodesInRestTree restEntries)
          restTree
      header : metasWithoutHeader = metas
      (localMetas, globalMetas) = partition existsInRestTree metasWithoutHeader
   in (header : globalMetas, VertexTree localMetas vertices restTree ttype)
  where
    subNodesInRestTree (VertexTree subMetas _ restTree' _) =
      case restTree' of
        Just restSubMetas -> subNodesInRestTree restSubMetas <> subMetas
        Nothing -> subMetas

updateVertexNames
  :: Int -> VertexTreeEntry -> (Int, VertexTreeEntry)
updateVertexNames index (VertexEntry vertex) =
  let newIndex = index + 1
      vertexPrefix = dropIndex . vName $ vertex
      vertexName = vertexPrefix <> T.pack (show index)
      renamedVertex = VertexEntry (vertex {vName = vertexName})
   in (newIndex, renamedVertex)
updateVertexNames index entry = (index, entry)

link :: Maybe VertexTree -> Maybe VertexTree -> Maybe VertexTree
link Nothing acc = acc
link (Just t) Nothing = Just t
link (Just t) (Just rest) = Just t {tRest = Just rest}

fromVertexTree :: VertexTree -> VertexZipper
fromVertexTree = (`VertexZipper` [])

goUpAll :: VertexZipper -> VertexZipper
goUpAll z@(VertexZipper _ []) = z
goUpAll (VertexZipper cur (VertexContext meta before after ptype restAbove : ps)) =
  let combinedEntries = before ++ NE.toList (tVertexNodes cur) ++ after
      parent = VertexTree meta (NE.fromList combinedEntries) restAbove ptype
   in goUpAll (VertexZipper parent ps)

toVertexTreeFromZ :: VertexZipper -> VertexTree
toVertexTreeFromZ = zFocus . goUpAll

processZipperFocus :: VertexZipper -> Maybe VertexZipper
processZipperFocus (VertexZipper cur path) = do
  fixedRest <- case tRest cur of
    Nothing -> Just Nothing
    Just r -> moveVerticesInVertexTree r >>= (Just . Just)

  let entriesList = NE.toList (tVertexNodes cur)
      groupsHere = groupVertexWithLeadingComments entriesList
      groupsRest =
        maybe [] (groupVertexWithLeadingComments . NE.toList . tVertexNodes) fixedRest
      allGroups = groupsHere ++ groupsRest

      groupsByType :: M.Map VertexTreeType [CommentGroup]
      groupsByType = M.fromListWith (++) [(determineGroup (vX (cVertex g)), [g]) | g <- allGroups]

      allTypes = [LeftTree, MiddleTree, RightTree, SupportTree]
      makeTreeFor t =
        let gs = M.findWithDefault [] t groupsByType
         in if null gs
              then Nothing
              else
                Just
                  ( VertexTree
                      (tMetaNodes cur)
                      (NE.fromList (concatMap (\g -> cComments g ++ [VertexEntry (cVertex g)]) gs))
                      Nothing
                      t
                  )

      treesForTypes = mapMaybe makeTreeFor allTypes

      (rootTreeM, restTrees) =
        case partition (\(VertexTree _ _ _ t) -> t == tType cur) treesForTypes of
          ([r], rs) -> (Just r, rs)
          ([], rs) -> (listToMaybe rs, filter (\x -> tType x /= tType (head rs)) rs)
          _ -> error "Flera root-träd med samma typ, oväntat"

      linkedRest = foldr (link . Just) Nothing restTrees

  case rootTreeM of
    Just rootTree ->
      let newFocus = rootTree {tRest = linkedRest}
       in Just (VertexZipper newFocus path)
    Nothing ->
      case treesForTypes of
        [] -> Nothing
        (firstTree : others) ->
          let newFocus = firstTree {tRest = foldr (link . Just) Nothing others}
           in Just (VertexZipper newFocus path)

moveVerticesInVertexTree :: VertexTree -> Maybe VertexTree
moveVerticesInVertexTree root =
  let startZ = fromVertexTree root
   in do
        z' <- processZipperFocus startZ
        pure $ toVertexTreeFromZ z'

updateVertexTree :: VertexTree -> VertexTree
updateVertexTree vertexTree =
  let (globalMetas, vertexTree') = getVertexTreeGlobals vertexTree
      Just updatedVertexTree = moveVerticesInVertexTree vertexTree'
      addGlobalMetas (VertexTree metas vertices restTree ttype) =
        VertexTree (globalMetas ++ metas) vertices restTree ttype
   in sortVertices . addGlobalMetas $ updatedVertexTree

groupByMeta :: [VertexTreeEntry] -> [[VertexTreeEntry]]
groupByMeta [] = []
groupByMeta (x : xs)
  | isMeta x =
      let (grp, rest) = break isMeta xs
       in (x : grp) : groupByMeta rest
  | otherwise =
      case groupByMeta xs of
        [] -> [[x]]
        (g : gs) -> (x : g) : gs

isMeta :: VertexTreeEntry -> Bool
isMeta (VertexEntry _) = False
isMeta _ = True

groupVertexWithLeadingComments :: [VertexTreeEntry] -> [CommentGroup]
groupVertexWithLeadingComments = go []
  where
    go _ [] = []
    go acc (entry : rest) =
      case entry of
        CommentEntry _ -> go (acc ++ [entry]) rest
        VertexEntry v ->
          CommentGroup {cComments = acc, cVertex = v} : go [] rest
        _ -> go [] rest

sortCommentGroups :: [CommentGroup] -> [CommentGroup]
sortCommentGroups = sortOn (\cg -> (vZ (cVertex cg), vY (cVertex cg)))

ungroupCommentGroups :: [CommentGroup] -> [VertexTreeEntry]
ungroupCommentGroups =
  concatMap (\cg -> cComments cg ++ [VertexEntry (cVertex cg)])

processGroup :: [VertexTreeEntry] -> [VertexTreeEntry]
processGroup [] = []
processGroup (metaOrHeader : rest) =
  let commentGroups = groupVertexWithLeadingComments rest
      sortedGroups = sortCommentGroups commentGroups
      entriesSorted = ungroupCommentGroups sortedGroups
   in metaOrHeader : entriesSorted

sortVertices :: VertexTree -> VertexTree
sortVertices (VertexTree metas nodes maybeRest ttype) =
  let groups = groupByMeta (NE.toList nodes)
      processedGroups = map processGroup groups
      nodesSorted = concat processedGroups
      (_, nodes') = TR.mapAccumL updateVertexNames 0 (NE.fromList nodesSorted)
   in VertexTree metas nodes' (sortVertices <$> maybeRest) ttype

verticesQuery :: NP.NodePath
verticesQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "nodes"]

possiblyVertex :: VertexTreeEntry -> Maybe Vertex
possiblyVertex (VertexEntry v) = Just v
possiblyVertex _ = Nothing

getVertexNamesInTree
  :: VertexTree -> M.Map (Scientific, Scientific, Scientific) Text
getVertexNamesInTree vertexTree@(VertexTree {tVertexNodes = vs}) =
  let vertexCordNamePair vertex =
        ((vX vertex, vY vertex, vZ vertex), vName vertex)
      getVertexNames =
        M.fromList
          . mapMaybe (fmap vertexCordNamePair . possiblyVertex)
          . NE.toList
      restNames =
        case vertexTree of
          VertexTree {tRest = Just r} -> getVertexNamesInTree r
          VertexTree {tRest = Nothing} -> M.empty
   in M.union (getVertexNames vs) restNames

isObjectKeyEqual :: NP.NodeSelector -> Node -> Bool
isObjectKeyEqual (NP.ObjectKey a) (ObjectKey (String b, _)) = a == b
isObjectKeyEqual _ _ = False

vertexTreeToNodeVector :: VertexTree -> Vector Node
vertexTreeToNodeVector (VertexTree metas vertices maybeOtherTree _) =
  let currentNodes = V.fromList $ metas ++ (NE.toList . NE.map vertexEntryToNode $ vertices)
      otherNodes = maybe V.empty vertexTreeToNodeVector maybeOtherTree
      vertexEntryToNode entry =
        case entry of
          (CommentEntry node) -> node
          (MetaEntry node) -> node
          (VertexEntry vertex) ->
            let name = String . vName $ vertex
                x = Number . vX $ vertex
                y = Number . vY $ vertex
                z = Number . vZ $ vertex
                possiblyMeta = maybe [] (pure . Object) (vMeta vertex)
             in Array . fromList $ [name, x, y, z] ++ possiblyMeta
   in currentNodes <> otherNodes

updateVerticesInNode :: NP.NodePath -> VertexTree -> Node -> Node
updateVerticesInNode (NP.NodePath Empty) g (Array _) =
  Array (vertexTreeToNodeVector g)
updateVerticesInNode (NP.NodePath ((NP.ArrayIndex i) :<| qrest)) g (Array children) =
  let updateInNode nodeToUpdate =
        children
          // [(i, updateVerticesInNode (NP.NodePath qrest) g nodeToUpdate)]
   in Array $ maybe children updateInNode (children !? i)
updateVerticesInNode (NP.NodePath ((NP.ObjectIndex i) :<| qrest)) g (Object children) =
  let updateInNode _ =
        children
          // [(i, updateVerticesInNode (NP.NodePath qrest) g (children ! i))]
   in Object $ maybe children updateInNode (children !? i)
updateVerticesInNode (NP.NodePath (k@(NP.ObjectKey _) :<| qrest)) g (Object children) =
  let updateInNode i =
        children
          // [(i, updateVerticesInNode (NP.NodePath qrest) g (children ! i))]
   in Object . maybe children updateInNode $
        V.findIndex (isObjectKeyEqual k) children
updateVerticesInNode query g (ObjectKey (k, v)) =
  ObjectKey (k, updateVerticesInNode query g v)
updateVerticesInNode _ _ a = a

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

transform :: NC.NodeCursor -> Node -> Either Text Node
transform cursor topNode = getVertexTree verticesQuery topNode >>= go
  where
    go vertexTree =
      let vertexNames = getVertexNamesInTree vertexTree
          updatedVertexTree = updateVertexTree vertexTree
          updatedVertexNames = getVertexNamesInTree updatedVertexTree
          updateMap = M.fromList $ on zip M.elems vertexNames updatedVertexNames
       in Right . findAndUpdateTextInNode updateMap cursor $
            updateVerticesInNode verticesQuery updatedVertexTree topNode
