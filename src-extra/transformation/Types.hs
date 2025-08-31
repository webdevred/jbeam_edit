module Types (
  VertexForest,
  VertexTree (..),
  VertexTreeType (..),
  VertexTreeEntry (..),
  Vertex (..),
  CommentGroup (..),
  MetaMap,
) where

import Core.Node
import Data.Scientific (Scientific)

type VertexForest = Map VertexTreeType VertexTree

data VertexTreeType
  = LeftTree
  | MiddleTree
  | RightTree
  | SupportTree
  deriving (Eq, Ord, Show)

data VertexTreeEntry
  = VertexEntry Vertex
  | CommentEntry InternalComment
  | MetaEntry Object
  deriving (Eq, Show)

data VertexTree = VertexTree
  { tMetaNodes :: [Node]
  , tVertexNodes :: NonEmpty VertexTreeEntry
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
  { cComments :: [InternalComment]
  , cVertex :: Vertex
  , cMeta :: MetaMap
  }
  deriving (Show)

type MetaMap = Map Text Node
