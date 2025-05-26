module Transformation (
  transform,
) where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Core.Node
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

data VertexTreeType
  = LeftTree
  | MiddleTree
  | RightTree
  | SupportTree
  deriving (Eq, Ord, Show)

data VertexTreeEntry
  = VertexEntry Vertex
  | MetaEntry Node
  | MiscEntry Node

data VertexTree = VertexTree
  { tNodes :: NonEmpty VertexTreeEntry
  , tRest :: Maybe VertexTree
  , tType :: VertexTreeType
  }

data Vertex = Vertex
  { vName :: Text
  , vX :: Scientific
  , vY :: Scientific
  , vZ :: Scientific
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
  let verticeName = vName <$> newVertice node
   in verticeName == Just verticePrefix

getFirstVerticeName :: [Node] -> Text
getFirstVerticeName (node:_) = vName . fromJust . newVertice $ node

breakVertices :: Text -> [Node] -> ([Node], [Node])
breakVertices verticePrefix = f []
  where
    typeForVerticeList = mostCommon . NE.map (determineGroup . vX)
    f acc nodes =
      case nodes of
        [] -> (acc, [])
        (node : rest)
          | hasVerticePrefix verticePrefix node -> (node : acc, nodes)
          | isNonVertice node -> (node : acc, nodes)
          | isVertice node ->
              let (metaNodesNext, currentNodes) = span isNonVertice acc
               in (currentNodes, metaNodesNext ++ (node : rest))

toVertexTreeEntry :: Node -> VertexTreeEntry
toVertexTreeEntry node
  | isJust vertice = VertexEntry (fromJust vertice)
  | isNothing vertice && isObjectNode node = MetaEntry node
  | otherwise = MiscEntry node
  where
    vertice = newVertice node

typeForNodes = undefined

nodesListToTree :: NonEmpty Node -> VertexTree
nodesListToTree nodes =
  let (nonVertices, rest) = NE.break isNonVertice nodes
      verticePrefix = getFirstVerticeName rest
      (vertices, rest') = breakVertices verticePrefix rest
   in VertexTree
        { tNodes =
            NE.fromList
              (map toVertexTreeEntry (nonVertices ++ reverse vertices))
        , tRest = nodesListToTree <$> NE.nonEmpty rest'
        , tType = typeForNodes vertices
        }

getVertexTree :: Node -> Either Node VertexTree
getVertexTree node =
  case node of
    Array ns
      | null ns -> Left node
      | otherwise -> Right . nodesListToTree . NE.fromList . V.toList $ ns
    bad -> Left bad

transform = undefined
