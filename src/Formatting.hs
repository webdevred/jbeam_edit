module Formatting
  ( formatNode
  , newRuleSet
  , RuleSet(..)
  ) where

import Core.Node (Node(..), isCommentNode)
import Core.NodeCursor qualified as NC
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Scientific (FPFormat(Fixed), formatScientific)
import Data.Text qualified as T
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V (null, toList)
import Formatting.Rules
  ( RuleSet(..)
  , applyPadLogic
  , findPropertiesForCursor
  , newRuleSet
  )

addDelimiters ::
     RuleSet -> Int -> NC.NodeCursor -> Bool -> [Text] -> [Node] -> [Text]
addDelimiters _ _ _ _ acc [] = acc
addDelimiters rs index c complexChildren acc ns@(node:rest)
  | complexChildren && null acc =
    addDelimiters rs index c complexChildren ["\n"] ns
  | isCommentNode node =
    addDelimiters
      rs
      newIndex
      c
      complexChildren
      (formatNode rs c node <> "\n" : acc)
      rest
  | otherwise =
    let new_acc = T.concat [applyCrumbAndFormat, comma, space, newline] : acc
     in addDelimiters rs newIndex c complexChildren new_acc rest
  where
    applyCrumbAndFormat =
      NC.applyCrumb (NC.ArrayIndex index) c (formatNode rs) node
    newIndex = index + 1
    comma = bool "," "" $ null rest
    space = bool " " "" $ null rest || complexChildren
    newline = bool "" "\n" complexChildren

isComplexNode :: Node -> Bool
isComplexNode (Object _) = True
isComplexNode (Array _) = True
isComplexNode (ObjectKey (_key, val)) = isComplexNode val
isComplexNode _ = False

isNumberNode :: Node -> Bool
isNumberNode (Number _) = True
isNumberNode _ = False

indent :: Text -> Text
indent s
  | T.all isSpace s = s
  | otherwise = "  " <> s

doFormatNode :: RuleSet -> NC.NodeCursor -> Vector Node -> Text
doFormatNode rs cursor nodes =
  let formatted =
        reverse . addDelimiters rs 0 cursor complexChildren [] . V.toList
          $ nodes
   in if complexChildren
        then T.unlines . map indent . concatMap T.lines $ formatted
        else T.concat formatted
  where
    complexChildren = any isComplexNode nodes

padLogic :: (Node -> Text) -> Int -> Bool -> Node -> Text
padLogic f padAmount padZeros n
  | not (isComplexNode n) =
    if padZeros && isNumberNode n
      then T.justifyLeft padAmount '0' $ f n
      else T.justifyRight padAmount ' ' $ f n
  | otherwise = f n

formatScalarNode :: Node -> Text
formatScalarNode (SinglelineComment c) = "\n// " <> c
formatScalarNode (MultilineComment c) = T.concat ["/* ", c, " */"]
formatScalarNode (String s) = T.concat ["\"", s, "\""]
formatScalarNode (Number n) = T.pack . formatScientific Fixed Nothing $ n
formatScalarNode (Bool True) = "true"
formatScalarNode (Bool _) = "false"
formatScalarNode Null = "null"
formatScalarNode _ = error "Unhandled scalar node"

formatNode :: RuleSet -> NC.NodeCursor -> Node -> Text
formatNode rs cursor (Array a)
  | V.null a = "[]"
  | otherwise = T.concat ["[", doFormatNode rs cursor a, "]"]
formatNode rs cursor (Object o)
  | V.null o = "{}"
  | otherwise = T.concat ["{", doFormatNode rs cursor o, "}"]
formatNode rs cursor (ObjectKey (k, v)) =
  T.concat
    [formatNode rs cursor k, " : ", NC.applyObjCrumb k cursor (formatNode rs) v]
formatNode rs cursor n =
  let ps = findPropertiesForCursor cursor rs
   in applyPadLogic (padLogic formatScalarNode) ps n
