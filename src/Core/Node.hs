module Core.Node (
  isCommentNode,
  isObjectNode,
  isNumberNode,
  isComplexNode,
  Node (..),
) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)

type Object = Vector Node

type ObjectKey = (Node, Node)

type Array = Vector Node

data Node
  = Array Array
  | Object Object
  | ObjectKey ObjectKey
  | String Text
  | Number Scientific
  | Bool Bool
  | SinglelineComment Text
  | MultilineComment Text
  | Null
  deriving (Eq, Read, Show)

isCommentNode :: Node -> Bool
isCommentNode (MultilineComment _) = True
isCommentNode (SinglelineComment _) = True
isCommentNode _ = False

isObjectNode :: Node -> Bool
isObjectNode (Object _) = True
isObjectNode _ = False

isNumberNode :: Node -> Bool
isNumberNode (Number _) = True
isNumberNode _ = False

isComplexNode :: Node -> Bool
isComplexNode (Object _) = True
isComplexNode (Array _) = True
isComplexNode (ObjectKey (_key, val)) = isComplexNode val
isComplexNode _ = False
