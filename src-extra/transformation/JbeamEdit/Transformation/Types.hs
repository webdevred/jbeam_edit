module JbeamEdit.Transformation.Types (
  anVertexName,
  VertexForest,
  VertexTree (..),
  VertexTreeType (..),
  Vertex (..),
  AnnotatedVertex (..),
  VertexTreeKey (..),
  MetaMap,
  VertexConnMap,
  UpdateNamesMap,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml.Aeson (
  FromJSON (..),
  withText,
 )
import JbeamEdit.Core.Node
import JbeamEdit.Transformation.OMap1

data VertexTreeKey = SupportKey | PrefixKey Text deriving (Eq, Ord, Show)

type VertexForest = Map VertexTreeType (OMap1 VertexTreeKey VertexTree)

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
      _ -> fail $ "Unknown VertexTreeType: " ++ T.unpack t

data VertexTree = VertexTree
  { tComments :: [InternalComment]
  , tAnnotatedVertices :: NonEmpty AnnotatedVertex
  }
  deriving (Show)

data Vertex = Vertex
  { vName :: Text
  , vX, vY, vZ :: Scientific
  , vMeta :: Maybe Object
  }
  deriving (Eq, Show)

data AnnotatedVertex = AnnotatedVertex
  { aComments :: [InternalComment]
  , aVertex :: Vertex
  , aMeta :: MetaMap
  }
  deriving (Eq, Show)

anVertexName :: AnnotatedVertex -> Text
anVertexName = vName . aVertex

type MetaMap = Map Text Node

type VertexConnMap = Map Text (VertexTreeType, Int)

type UpdateNamesMap = Map Text Text
