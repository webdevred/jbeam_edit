module JbeamEdit.Core.Node (
  expectArray,
  isCommentNode,
  isObjectNode,
  isObjectKeyNode,
  isNumberNode,
  isStringNode,
  maybeObjectKey,
  isSinglelineComment,
  commentIsAttachedToPreviousNode,
  isComplexNode,
  extractPreviousAssocCmt,
  possiblyChildren,
  numberValueToScientific,
  scientificToText,
  mkNumberValue,
  mkNumberValueNormalized,
  Node (..),
  NumberValue (..),
  ArrayValue (..),
  ObjectValue (..),
  InternalComment (..),
  AssociationDirection (..),
  mkArray,
  mkObject,
) where

import Control.Applicative ((<|>))
import Data.Scientific (
  FPFormat (Fixed),
  Scientific,
  formatScientific,
  isInteger,
 )
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V

data ArrayValue = ArrayValue
  { avElements :: Vector Node
  , avTrailingComma :: Bool
  }
  deriving (Eq, Ord, Read, Show)

data ObjectValue = ObjectValue
  { ovElements :: Vector Node
  , ovTrailingComma :: Bool
  }
  deriving (Eq, Ord, Read, Show)

type ObjectKey = (Node, Node)

data AssociationDirection = PreviousNode | NextNode
  deriving (Eq, Ord, Read, Show)

data InternalComment = InternalComment
  { cText :: Text
  , cMultiline :: Bool
  , cAssociationDirection :: AssociationDirection
  , cHadNewlineBefore :: Bool
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
data NumberValue = NumberValue
  { nvText :: Text
  , nvValue :: Scientific
  }
  deriving (Eq, Ord, Read, Show)

data Node
  = Array ArrayValue
  | Object ObjectValue
  | ObjectKey ObjectKey
  | String Text
  | Number NumberValue
  | Bool Bool
  | Comment InternalComment
  | Null
  deriving (Eq, Ord, Read, Show)

commentIsAttachedToPreviousNode :: InternalComment -> Bool
commentIsAttachedToPreviousNode = (==) PreviousNode . cAssociationDirection

extractPreviousAssocCmt
  :: [Node]
  -> (Maybe InternalComment, [Node])
extractPreviousAssocCmt (Comment cmt : ns)
  | commentIsAttachedToPreviousNode cmt = (Just cmt, ns)
extractPreviousAssocCmt ns = (Nothing, ns)

maybeObjectKey :: Node -> Maybe Text
maybeObjectKey (ObjectKey (String key, _)) = Just key
maybeObjectKey _ = Nothing

isCommentNode :: Node -> Bool
isCommentNode (Comment _) = True
isCommentNode _ = False

isObjectNode :: Node -> Bool
isObjectNode (Object _) = True
isObjectNode _ = False

mkArray :: Vector Node -> Node
mkArray v = Array (ArrayValue v False)

mkObject :: Vector Node -> Node
mkObject v = Object (ObjectValue v False)

isObjectKeyNode :: Node -> Bool
isObjectKeyNode (ObjectKey _) = True
isObjectKeyNode _ = False

isStringNode :: Node -> Bool
isStringNode (String _) = True
isStringNode _ = False

isNumberNode :: Node -> Bool
isNumberNode (Number _) = True
isNumberNode _ = False

numberValueToScientific :: NumberValue -> Scientific
numberValueToScientific = nvValue

scientificToText :: Scientific -> Text
scientificToText v
  | isInteger v = T.pack $ show (floor v :: Integer)
  | otherwise = T.pack $ formatScientific Fixed Nothing v

mkNumberValue :: Text -> Scientific -> NumberValue
mkNumberValue = NumberValue

mkNumberValueNormalized :: Scientific -> NumberValue
mkNumberValueNormalized v = NumberValue {nvText = scientificToText v, nvValue = v}

isSinglelineComment :: Node -> Bool
isSinglelineComment (Comment (InternalComment _ False _ _)) = True
isSinglelineComment _ = False

expectArray :: Node -> Maybe (Vector Node)
expectArray (Array av) = Just (avElements av)
expectArray _ = Nothing

expectObject :: Node -> Maybe (Vector Node)
expectObject (Object ov) = Just (ovElements ov)
expectObject _ = Nothing

possiblyChildren :: Node -> Maybe (Vector Node)
possiblyChildren n = expectArray n <|> expectObject n

moreNodesThanOne :: Vector Node -> Bool
moreNodesThanOne v
  | len == 1 = any moreNodesThanOne . possiblyChildren $ V.unsafeHead v
  | len > 1 = True
  | otherwise = False
  where
    len = V.length v

{- | isComplexNode

  Determines whether a given JBEAM node is "complex".

  A node is considered complex if it contains, at any level, an array or object
  with more than one child node, ignoring chains of arrays or objects that contain
  exactly one child.

  Scalar values (numbers, strings, booleans, null, comments) are never complex.
  `ObjectKey` nodes delegate complexity to their value.

  Examples in AST form:

    -- [1] → not complex
    mkArray (fromList [Number 1])

    -- [[1]] → not complex
    mkArray (fromList [mkArray (fromList [Number 1])])

    -- [1,2] → complex
    mkArray (fromList [Number 1, Number 2])

    -- { a: 1 } → not complex
    mkObject (fromList [ObjectKey (String "a", Number 1)])

    -- { a: 1, b: 2 } → complex
    mkObject (fromList [ ObjectKey (String "a", Number 1)
                       , ObjectKey (String "b", Number 2)
                       ])

    -- { a: [1] } → not complex
    mkObject (fromList [ObjectKey (String "a", mkArray (fromList [Number 1]))])

    -- { a: [1,2] } → complex
    mkObject (fromList [ObjectKey (String "a", mkArray (fromList [Number 1, Number 2]))])
-}
isComplexNode :: Node -> Bool
isComplexNode (Object ov) = moreNodesThanOne (ovElements ov)
isComplexNode (Array av) = moreNodesThanOne (avElements av)
isComplexNode (ObjectKey (_key, val)) = isComplexNode val
isComplexNode _ = False
