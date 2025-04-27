module Formatting
  ( formatNode
  ) where

import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Scientific (FPFormat(Fixed), formatScientific)
import Data.Text qualified as T
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V (null, toList)

import NodeCursor qualified as NC
import Parsing (Node(..), isCommentNode)

addDelimiters :: Int -> NC.NodeCursor -> Bool -> [Text] -> [Node] -> [Text]
addDelimiters _ _ _ acc [] = acc
addDelimiters index c complexChildren acc ns@(node:rest)
  | complexChildren && null acc =
    addDelimiters index c complexChildren ["\n"] ns
  | isCommentNode node =
    addDelimiters
      newIndex
      c
      complexChildren
      (formatNode c node <> "\n" : acc)
      rest
  | otherwise =
    let new_acc = T.concat [applyCrumbAndFormat, comma, space, newline] : acc
     in addDelimiters newIndex c complexChildren new_acc rest
  where
    applyCrumbAndFormat = NC.applyCrumb (NC.ArrayIndex index) c formatNode node
    newIndex = index + 1
    comma = bool "," "" $ null rest
    space = bool " " "" $ null rest || complexChildren
    newline = bool "" "\n" complexChildren

isComplexNode :: Node -> Bool
isComplexNode (Object _) = True
isComplexNode (Array _) = True
isComplexNode (ObjectKey (_key, val)) = isComplexNode val
isComplexNode _ = False

indent :: Text -> Text
indent s
  | T.all isSpace s = s
  | otherwise = "  " <> s

doFormatNode :: NC.NodeCursor -> Vector Node -> Text
doFormatNode cursor nodes =
  let formatted =
        reverse . addDelimiters 0 cursor complexChildren [] . V.toList $ nodes
   in if complexChildren
        then T.unlines . map indent . concatMap T.lines $ formatted
        else T.concat formatted
  where
    complexChildren = any isComplexNode nodes

formatNode :: NC.NodeCursor -> Node -> Text
formatNode _ (SinglelineComment c) = "\n// " <> c
formatNode _ (MultilineComment c) = T.concat ["/* ", c, " */"]
formatNode _ (String s) = T.concat ["\"", s, "\""]
formatNode _ (Number n) = T.pack . formatScientific Fixed Nothing $ n
formatNode _ (Bool True) = "true"
formatNode _ (Bool _) = "false"
formatNode _ Null = "null"
formatNode cursor (Array a)
  | V.null a = "[]"
  | otherwise = T.concat ["[", doFormatNode cursor a, "]"]
formatNode cursor (Object o)
  | V.null o = "{}"
  | otherwise = T.concat ["{", doFormatNode cursor o, "}"]
formatNode cursor (ObjectKey (k, v)) =
  T.concat [formatNode cursor k, " : ", NC.applyObjCrumb k cursor formatNode v]
