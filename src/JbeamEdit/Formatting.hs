module JbeamEdit.Formatting (
  formatNode,
  formatWithCursor,
  formatScalarNode,
  newRuleSet,
  RuleSet (..),
) where

import Control.Monad (guard)
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.List (uncons)
import Data.Scientific (FPFormat (Fixed), formatScientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V (null, toList)
import JbeamEdit.Core.Node (
  InternalComment (..),
  Node (..),
  commentIsAttachedToPreviousNode,
  isCommentNode,
  isComplexNode,
 )
import JbeamEdit.Core.NodeCursor (newCursor)
import JbeamEdit.Core.NodeCursor qualified as NC
import JbeamEdit.Formatting.Rules (
  RuleSet (..),
  applyPadLogic,
  findPropertiesForCursor,
  lookupIndentProperty,
  newRuleSet,
  noComplexNewLine,
 )

addDelimiters
  :: RuleSet -> Int -> NC.NodeCursor -> Bool -> [Text] -> [Node] -> [Text]
addDelimiters _ _ _ _ acc [] = acc
addDelimiters rs index c complexChildren acc ns@(node : rest)
  | complexChildren && null acc =
      addDelimiters rs index c complexChildren ["\n"] ns
  | isCommentNode node =
      let formatted = ("\n" <> formatWithCursor rs c node <> "\n") : acc
       in addDelimiters rs index c complexChildren formatted rest
  | otherwise =
      case assocPriorComment of
        Just (comment, rest') ->
          let formatted =
                (applyCrumbAndFormat <> ", " <> formatComment comment : acc)
           in addDelimiters rs index c complexChildren formatted rest'
        Nothing ->
          let new_acc = T.concat [applyCrumbAndFormat, comma, space, newline] : acc
           in addDelimiters rs newIndex c complexChildren new_acc rest
  where
    assocPriorComment = do
      (Comment cmt, rest') <- uncons rest
      guard (commentIsAttachedToPreviousNode cmt)
      pure (cmt, rest')

    applyCrumbAndFormat = NC.applyCrumb c (formatWithCursor rs) index node
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
formatComment (InternalComment {cMultiline = False, cText = c}) = "// " <> c
formatComment (InternalComment {cMultiline = True, cText = c}) = T.concat ["/* ", c, " */"]

formatScalarNode :: Node -> Text
formatScalarNode (String s) = T.concat ["\"", s, "\""]
formatScalarNode (Number n) = T.pack (formatScientific Fixed Nothing n)
formatScalarNode (Bool True) = "true"
formatScalarNode (Bool _) = "false"
formatScalarNode Null = "null"
formatScalarNode _ = error "Unhandled scalar node"

formatWithCursor :: RuleSet -> NC.NodeCursor -> Node -> Text
formatWithCursor rs cursor (Array a)
  | V.null a = "[]"
  | otherwise = T.concat ["[", doFormatNode rs cursor a, "]"]
formatWithCursor rs cursor (Object o)
  | V.null o = "{}"
  | otherwise = T.concat ["{", doFormatNode rs cursor o, "}"]
formatWithCursor rs cursor (ObjectKey (k, v)) =
  let formatWithKeyContext = NC.applyObjCrumb k cursor (formatWithCursor rs)
   in T.concat [formatWithKeyContext k, " : ", formatWithKeyContext v]
formatWithCursor _ _ (Comment comment) = formatComment comment
formatWithCursor rs cursor n =
  let ps = findPropertiesForCursor cursor rs
   in applyPadLogic formatScalarNode ps n

formatNode :: RuleSet -> Node -> Text
formatNode rs node = formatWithCursor rs newCursor node <> T.singleton '\n'
