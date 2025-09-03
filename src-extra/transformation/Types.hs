module Types (
  VertexForest,
  VertexTree (..),
  VertexTreeType (..),
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

data VertexTree = VertexTree
  { tComments :: [InternalComment]
  , tCommentGroups :: NonEmpty CommentGroup
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
