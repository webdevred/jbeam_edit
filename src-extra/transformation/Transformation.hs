module Transformation (
  transform,
) where

import Core.Node
import Data.Char (isDigit)
import Data.Function (on)
import Data.List (partition, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Scientific (Scientific)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Vector (Vector, (!), (!?), (//))
import GHC.IsList (fromList)

import Core.NodeCursor qualified as NC
import Core.NodePath qualified as NP
import Data.Foldable qualified as F (maximumBy)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
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

data VertexTreeContext = VertexTreeContext
  { ctxAboveMeta :: [Node]
  , ctxBefore :: [VertexTreeEntry]
  , ctxAfter :: [VertexTreeEntry]
  , ctxParentType :: VertexTreeType
  , ctxRest :: Maybe VertexTree
  , ctxAbove :: [VertexTreeContext]
  }
  deriving (Show)

data VertexTreeZipper = VertexTreeZipper
  { zFocus :: VertexTreeEntry
  , zMeta :: [Node]
  , zType :: VertexTreeType
  , zContext :: VertexTreeContext
  }
  deriving (Show)

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

newVertice :: Node -> Maybe Vertex
newVertice (Array ns) = f (V.toList ns)
  where
    f [String name, Number x, Number y, Number z, Object m] =
      Just (Vertex {vName = name, vX = x, vY = y, vZ = z, vMeta = Just m})
    f [String name, Number x, Number y, Number z] =
      Just (Vertex {vName = name, vX = x, vY = y, vZ = z, vMeta = Nothing})
    f _ = Nothing
newVertice _ = Nothing

isVertice :: Node -> Bool
isVertice node = isJust (newVertice node)

isNonVertice :: Node -> Bool
isNonVertice node = isNothing (newVertice node)

dropIndex :: Text -> Text
dropIndex = T.dropWhileEnd isDigit

hasVerticePrefix :: Text -> Node -> Bool
hasVerticePrefix verticePrefix node =
  let verticeName = dropIndex . vName <$> newVertice node
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
      | otherwise -> CommentEntry node

mostCommon :: NonEmpty VertexTreeType -> VertexTreeType
mostCommon = NE.head . F.maximumBy (compare `on` length) . NE.group1 . NE.sort

-- TODO: refactor to use not NE.fromList
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
            { tMetaNodes = nonVertices
            , tVertexNodes =
                NE.fromList (map toVertexTreeEntry (nonVertices ++ vertexNodes))
            , tRest = nodesListToTree <$> NE.nonEmpty rest'
            , tType = mostCommon $ NE.map (determineGroup . vX) vs
            }

-- TODO: refactor to use not NE.fromList
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
vertexInCorrectTree ttype vertex = ttype == determineGroup (vX vertex)

determineGroup :: Scientific -> VertexTreeType
determineGroup x
  | x < -0.09 = RightTree
  | x < 0.09 = MiddleTree
  | otherwise = LeftTree

isMetaOrVertexHasTreeType :: VertexTreeType -> VertexTreeEntry -> Bool
isMetaOrVertexHasTreeType vtype (VertexEntry vertex) =
  determineGroup (vX vertex) == vtype
isMetaOrVertexHasTreeType _ _ = True

zipperToVertexTree :: VertexTreeZipper -> VertexTree
zipperToVertexTree (VertexTreeZipper _ _ _ ctx) =
  fromMaybe (error "zipperToVertexTree: missing tree") (ctxRest ctx)

-- TODO: refactor to use not NE.fromList
vertexTreeToZipper :: VertexTree -> VertexTreeZipper
vertexTreeToZipper vt =
  let entries = tVertexNodes vt
      metas = tMetaNodes vt
      t = tType vt
   in VertexTreeZipper
        { zFocus = NE.head entries
        , zMeta = metas
        , zType = t
        , zContext =
            VertexTreeContext
              { ctxAboveMeta = []
              , ctxBefore = []
              , ctxAfter = NE.tail entries
              , ctxParentType = t
              , ctxRest = Just vt
              , ctxAbove = []
              }
        }

entryIsNonVertice :: VertexTreeEntry -> Bool
entryIsNonVertice (VertexEntry _) = False
entryIsNonVertice _ = True

metaKey (ObjectKey (key, _)) = Just key
metaKey _ = Nothing

-- TODO: refactor to use not NE.fromList
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
      (localMetas, globalMetas) = partition existsInRestTree metas
   in (globalMetas, VertexTree localMetas vertices restTree ttype)
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

moveVerticesInZipper :: VertexTreeZipper -> VertexTreeZipper
moveVerticesInZipper z@(VertexTreeZipper _ _ _ ctx) =
  let Just orig = ctxRest ctx
      metas = tMetaNodes orig
      entries = NE.toList (tVertexNodes orig)

      classify (VertexEntry v) = determineGroup (vX v)
      classify _ = SupportTree

      grouped =
        foldr
          ( \e m ->
              let t = classify e
               in M.insertWith (++) t [e] m
          )
          M.empty
          entries

      mkTree t = VertexTree metas (NE.fromList (M.findWithDefault [] t grouped)) Nothing t

      leftT = mkTree LeftTree
      midT = mkTree MiddleTree
      rightT = mkTree RightTree
      supportT = mkTree SupportTree

      newRoot =
        leftT
          { tRest = Just (midT {tRest = Just (rightT {tRest = Just supportT})})
          }
   in z {zContext = ctx {ctxRest = Just newRoot}}

updateVertexTree :: VertexTree -> VertexTree
updateVertexTree vertexTree =
  let (globalMetas, vertexTree') = getVertexTreeGlobals vertexTree
      updatedVertexTree = zipperToVertexTree . moveVerticesInZipper . vertexTreeToZipper $ vertexTree'
      addGlobalMetas (VertexTree metas vertices restTree ttype) =
        VertexTree (globalMetas ++ metas) vertices restTree ttype
   in sortVertices . addGlobalMetas $ updatedVertexTree

-- TODO: reimplement to use NonEmpty
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

-- TODO: reimplement to use NonEmpty

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

verticeQuery :: NP.NodePath
verticeQuery = fromList [NP.ObjectIndex 0, NP.ObjectKey "nodes"]

possiblyVertice :: VertexTreeEntry -> Maybe Vertex
possiblyVertice (VertexEntry v) = Just v
possiblyVertice _ = Nothing

getVertexNamesInTree
  :: VertexTree -> M.Map (Scientific, Scientific, Scientific) Text
getVertexNamesInTree vertexTree@(VertexTree {tVertexNodes = vs}) =
  let verticeCordNamePair vertice =
        ((vX vertice, vY vertice, vZ vertice), vName vertice)
      getVertexNames =
        M.fromList
          . mapMaybe (fmap verticeCordNamePair . possiblyVertice)
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
vertexTreeToNodeVector (VertexTree metas vertices maybeOtherTree ttype) =
  let currentNodes = V.fromList . NE.toList . NE.map vertexEntryToNode $ vertices
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

transform :: NC.NodeCursor -> Node -> Node
transform cursor topNode =
  let vertexTree = getVertexTree verticeQuery topNode
      vertexNames = getVertexNamesInTree vertexTree
      updatedVertexTree = updateVertexTree vertexTree
      updatedVertexNames = getVertexNamesInTree updatedVertexTree
      updateMap = M.fromList $ on zip M.elems vertexNames updatedVertexNames
   in findAndUpdateTextInNode updateMap cursor $
        updateVerticesInNode verticeQuery updatedVertexTree topNode
