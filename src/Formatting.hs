module Formatting
  ( formatNode
  ) where

import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V (null, toList)

import Parsing (Node(..))

whenTrue :: Bool -> Text -> Text
whenTrue True a = a
whenTrue False _ = ""

addDelimiters :: Bool -> [Text] -> [Node] -> [Text]
addDelimiters _ acc [] = acc
addDelimiters complexChildren acc ns@(node:rest)
  | complexChildren && null acc = addDelimiters complexChildren ["\n"] ns
  | isCommentNode node =
    addDelimiters complexChildren (T.append (formatNode node) "\n" : acc) rest
  | otherwise =
    let new_acc = T.concat [formatNode node, comma, space, newline] : acc
     in addDelimiters complexChildren new_acc rest
  where
    isCommentNode (MultilineComment _) = True
    isCommentNode (SinglelineComment _) = True
    isCommentNode _ = False
    comma = whenTrue (not . null $ rest) ","
    space = whenTrue ((not . null $ rest) && not complexChildren) " "
    newline = whenTrue complexChildren "\n"

isComplexNode :: Node -> Bool
isComplexNode (Object _) = True
isComplexNode (Array _) = True
isComplexNode (ObjectKey (_key, val)) = isComplexNode val
isComplexNode _ = False

indent :: Text -> Text
indent s
  | T.all isSpace s = s
  | otherwise = T.append "  " s

doFormatNode :: Vector Node -> Text
doFormatNode nodes =
  let formatted = reverse . addDelimiters complexChildren [] . V.toList $ nodes
   in if complexChildren
        then T.unlines . map indent . concatMap T.lines $ formatted
        else T.concat formatted
  where
    complexChildren = any isComplexNode nodes

formatNode :: Node -> Text
formatNode (SinglelineComment c) = T.append "\n// " c
formatNode (MultilineComment c) = T.concat ["/* ", c, " */"]
formatNode (String s) = T.concat ["\"", s, "\""]
formatNode (Number n) = T.pack . show $ n
formatNode (Bool True) = "true"
formatNode (Bool _) = "false"
formatNode (Array a)
  | V.null a = "[]"
  | otherwise = T.concat ["[", doFormatNode a, "]"]
formatNode (Object o)
  | V.null o = "{}"
  | otherwise = T.concat ["{", doFormatNode o, "}"]
formatNode (ObjectKey (k, v)) = T.concat [formatNode k, " : ", formatNode v]
