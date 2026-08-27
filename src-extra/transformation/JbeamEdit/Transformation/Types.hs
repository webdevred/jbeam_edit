module JbeamEdit.Transformation.Types (
  anVertexName,
  VertexForest,
  VertexTree (..),
  VertexTreeType (..),
  Vertex (..),
  vX,
  vY,
  vZ,
  AnnotatedVertex (..),
  VertexTreeKey (..),
  Beam (..),
  BeamPair (..),
  mkBeamPair,
  MetaMap,
  VertexConnMap,
  UpdateNamesMap,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
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
  , vXNum, vYNum, vZNum :: NumberValue
  , vMeta :: Maybe (Vector Node)
  }
  deriving (Show)

{- | A vertex keeps the number as the file wrote it, because it is written
back out unchanged and the formatter decides how it looks. Sorting and
grouping want the value, and read it through these.
-}
vX, vY, vZ :: Vertex -> Scientific
vX = nvValue . vXNum
vY = nvValue . vYNum
vZ = nvValue . vZNum

data AnnotatedVertex = AnnotatedVertex
  { aComments :: [InternalComment]
  , aVertex :: Vertex
  , aMeta :: MetaMap
  }
  deriving (Show)

anVertexName :: AnnotatedVertex -> Text
anVertexName = vName . aVertex

type MetaMap = Map Text Node

type VertexConnMap = Map Text (VertexTreeType, Int)

type UpdateNamesMap = Map Text Text

data BeamPair = BeamPair Text Text
  deriving (Eq, Ord, Show)

mkBeamPair :: Text -> Text -> BeamPair
mkBeamPair a b = BeamPair (min a b) (max a b)

data Beam = Beam
  { beamPair :: BeamPair
  , beamMeta :: MetaMap
  }
  deriving (Eq, Ord, Show)
