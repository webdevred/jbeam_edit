module JbeamEdit.Core.Node (
  isCommentNode,
  isObjectNode,
  isObjectKeyNode,
  isNumberNode,
  isStringNode,
  maybeObjectKey,
  isComplexNode,
  commentIsAttachedToPreviousNode,
  Node (..),
  InternalComment (..),
  AssociationDirection (..),
  Object,
  Array,
) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)

type Object = Vector Node

type ObjectKey = (Node, Node)

type Array = Vector Node

data AssociationDirection = PreviousNode | NextNode
  deriving (Eq, Ord, Read, Show)

data InternalComment = InternalComment
  { cText :: Text
  , cMultiline :: Bool
  , cAssociationDirection :: AssociationDirection
  }
  deriving (Eq, Ord, Read, Show)

{- | Node type
 Note that the concept of a `Node` here is different from "node" in jbeam, when I mention JBeam nodes in this document I will explicitly call them JBeam nodes or call them Vertices.
In this document when I refer to nodes I mean a datatype I which I have created to represent arrays, objects and scalar inside a JBeam structure.
type declares a type synomom for a already existing type while data declares a new type.
So a Node can be:
- a Array and contain a vector of Nodes
- a Object and contain a vector of Nodes
- a ObjectKey and contain a 2-element tuple (also called a pair) where the the first element is the key and the second is the value
- a String which contains a Text
- a Number which contains a Scientific, a numeric data type which I really like to due to it unlike most other types in many programming languages can express any arbitrary-precision decimal number without rounding errors or losing precision.
- a Bool which contains either True or False
- a MultilineComment or SinglelineComment which contains a Text
- a Null meaning that the node is empty

So in fact Node is a recursive data structure, a type which references itself.
Text is yet a another string type in Haskell which is good fit when doing appends or comparisons which I tend to do quite a lot in this codebase.
I comptemplated using something like [OMap](https://hackage.haskell.org/package/ordered-containers-0.2.4/docs/Data-Map-Ordered.html) for the Object but then I realized that I do not only need support pairs of keys and values for the Object case, I also need to support the Object having Comments as direct children.
The parser is currently written using attoaparsec but I are considering to migrate to Megaparsec since Megaparsec has better support implementing error messages which can be given to the end user.
-}
data Node
  = Array Array
  | Object Object
  | ObjectKey ObjectKey
  | String Text
  | Number Scientific
  | Bool Bool
  | Comment InternalComment
  | Null
  deriving (Eq, Ord, Read, Show)

commentIsAttachedToPreviousNode :: InternalComment -> Bool
commentIsAttachedToPreviousNode (InternalComment _ _ PreviousNode) = True
commentIsAttachedToPreviousNode (InternalComment _ _ NextNode) = False

maybeObjectKey :: Node -> Maybe Text
maybeObjectKey (ObjectKey (String key, _)) = Just key
maybeObjectKey _ = Nothing

isCommentNode :: Node -> Bool
isCommentNode (Comment _) = True
isCommentNode _ = False

isObjectNode :: Node -> Bool
isObjectNode (Object _) = True
isObjectNode _ = False

isObjectKeyNode :: Node -> Bool
isObjectKeyNode (ObjectKey _) = True
isObjectKeyNode _ = False

isStringNode :: Node -> Bool
isStringNode (String _) = True
isStringNode _ = False

isNumberNode :: Node -> Bool
isNumberNode (Number _) = True
isNumberNode _ = False

isComplexNode :: Node -> Bool
isComplexNode (Object _) = True
isComplexNode (Array _) = True
isComplexNode (ObjectKey (_key, val)) = isComplexNode val
isComplexNode _ = False
