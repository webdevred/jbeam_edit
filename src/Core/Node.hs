module Core.Node (
  isCommentNode,
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
  deriving (Eq, Show)

isCommentNode :: Node -> Bool
isCommentNode (MultilineComment _) = True
isCommentNode (SinglelineComment _) = True
isCommentNode _ = False
