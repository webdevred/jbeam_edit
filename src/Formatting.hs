module Formatting (
  formatNode,
  newRuleSet,
  RuleSet (..),
) where

import Core.Node (InternalComment (..), Node (..), isCommentNode, isComplexNode)
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Scientific (FPFormat (Fixed), formatScientific)
import Data.Text (Text)
import Data.Vector (Vector)
import Formatting.Rules (
  RuleSet (..),
  applyPadLogic,
  findPropertiesForCursor,
  lookupIndentProperty,
  newRuleSet,
  noComplexNewLine,
 )

import Core.NodeCursor qualified as NC
import Data.Text qualified as T
import Data.Vector qualified as V (null, toList)

addDelimiters
  :: RuleSet -> Int -> NC.NodeCursor -> Bool -> [Text] -> [Node] -> [Text]
addDelimiters _ _ _ _ acc [] = acc
addDelimiters rs index c complexChildren acc ns@(node : rest)
  | complexChildren && null acc =
      addDelimiters rs index c complexChildren ["\n"] ns
  | isCommentNode node =
      addDelimiters
        rs
        index
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

applyIndentation :: Int -> Text -> Text
applyIndentation n s
  | T.all isSpace s = s
  | otherwise = T.replicate n " " <> s

doFormatNode :: RuleSet -> NC.NodeCursor -> Vector Node -> Text
doFormatNode rs cursor nodes =
  let formatted =
        reverse . addDelimiters rs 0 cursor complexChildren [] . V.toList $
          nodes
      indentationAmount = lookupIndentProperty rs cursor
   in if complexChildren
        then
          T.unlines . map (applyIndentation indentationAmount) . concatMap T.lines $
            formatted
        else T.concat formatted
  where
    complexChildren =
      any isComplexNode nodes && not (noComplexNewLine rs cursor)

formatComment :: InternalComment -> Text
formatComment (InternalComment {cMultiline = True, cText = c}) = T.concat ["/* ", c, " */"]
formatComment (InternalComment {cMultiline = False, cText = c}) = "\n// " <> c

formatScalarNode :: Node -> Text
formatScalarNode (Comment c) = formatComment c
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
  let formatWithKeyContext = NC.applyObjCrumb k cursor (formatNode rs)
   in T.concat [formatWithKeyContext k, " : ", formatWithKeyContext v]
formatNode rs cursor n =
  let ps = findPropertiesForCursor cursor rs
   in applyPadLogic formatScalarNode ps n
