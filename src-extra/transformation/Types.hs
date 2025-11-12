module Types (
  VertexForest,
  VertexTree (..),
  VertexTreeType (..),
  Vertex (..),
  AnnotatedVertex (..),
  MetaMap,
  VertexConnMap,
  UpdateNamesMap,
) where

import Core.Node
import Data.Scientific (Scientific)
import Data.Yaml.Aeson (
  FromJSON (..),
  withText,
 )

type VertexForest = Map VertexTreeType VertexTree

data VertexTreeType
  = LeftTree
  | MiddleTree
  | RightTree
  | SupportTree
  deriving (Eq, Ord, Show)

instance FromJSON VertexTreeType where
  parseJSON = withText "VertexTreeType" $ \t ->
    case t of
      "LeftTree" -> pure LeftTree
      "MiddleTree" -> pure MiddleTree
      "RightTree" -> pure RightTree
      "SupportTree" -> fail "SupportTree not allowed in breakpoints"
      _ -> fail $ "Unknown VertexTreeType: " ++ toString t

data VertexTree = VertexTree
  { tComments :: [InternalComment]
  , tAnnotatedVertices :: NonEmpty (NonEmpty AnnotatedVertex)
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

data AnnotatedVertex = AnnotatedVertex
  { aComments :: [InternalComment]
  , aVertex :: Vertex
  , aMeta :: MetaMap
  }
  deriving (Eq, Show)

type MetaMap = Map Text Node

type VertexConnMap = Map Text (VertexTreeType, Int)

type UpdateNamesMap = Map Text Text
