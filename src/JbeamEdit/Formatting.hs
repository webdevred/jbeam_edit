{-# LANGUAGE CPP #-}

module JbeamEdit.Formatting (
  formatNode,
  formatWithCursor,
  formatScalarNode,
  formatNodeAndWrite,
  RuleSet (..),
) where

import Data.Bool (bool)
import Data.ByteString.Lazy qualified as LBS (fromStrict)
import Data.Char (isSpace)
import Data.Scientific (FPFormat (Fixed), formatScientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import Data.Vector qualified as V (null, toList)
import JbeamEdit.Core.Node (
  InternalComment (..),
  Node (..),
  extractPreviousAssocCmt,
  isCommentNode,
  isComplexNode,
  isObjectKeyNode,
  isSinglelineComment,
 )
import JbeamEdit.Core.NodeCursor (newCursor)
import JbeamEdit.Core.NodeCursor qualified as NC
import JbeamEdit.Formatting.Rules (
  RuleSet (..),
  applyPadLogic,
  findPropertiesForCursor,
  forceComplexNewLine,
  lookupIndentProperty,
  noComplexNewLine,
 )
import System.File.OsPath qualified as OS (writeFile)
import System.OsPath (OsPath)

splitTrailing :: Bool -> Text -> (Text, Text)
splitTrailing comma txt =
  let trailing = T.length (T.takeWhileEnd (== ' ') txt)
      trailing' = trailing - bool 0 1 comma
   in ( T.dropEnd trailing txt
      , T.replicate trailing' " "
      )

normalizeCommentNode :: Bool -> Node -> Node
normalizeCommentNode False (Comment (InternalComment txt False dir)) = Comment (InternalComment txt True dir)
normalizeCommentNode _ node = node

singleCharIf :: Char -> Bool -> Text
singleCharIf a True = T.singleton a
singleCharIf _ _ = ""

addDelimiters
  :: RuleSet -> Int -> NC.NodeCursor -> Bool -> [Text] -> [Node] -> [Text]
addDelimiters _ _ _ _ acc [] = acc
addDelimiters rs index c complexChildren acc ns@(node : rest)
  | complexChildren && null acc =
      addDelimiters rs index c complexChildren ["\n"] ns
  | isCommentNode node =
      let formattedComment = formatWithCursor rs c (normalizeCommentNode complexChildren node)
          formatted = (newlineBeforeComment <> formattedComment <> "\n") : acc
       in addDelimiters rs index c complexChildren formatted rest
  | otherwise =
      case extractPreviousAssocCmt rest of
        (Just comment, rest') ->
          let formatted =
                (applyCrumbAndFormat <> " " <> formatComment comment : acc)
           in addDelimiters rs index c complexChildren formatted rest'
        (Nothing, _) ->
          let new_acc =
                T.concat
                  [applyCrumbAndFormat, singleCharIf ' ' space, singleCharIf '\n' newline]
                  : acc
           in addDelimiters rs newIndex c complexChildren new_acc rest
  where
    newlineBeforeComment = bool "\n" "" $ any isObjectKeyNode rest || ["\n"] == acc
    applyCrumbAndFormat =
      let padded = NC.applyCrumb c (formatWithCursor rs) index node
          (formatted, spaces) = splitTrailing comma padded
       in formatted <> singleCharIf ',' comma <> spaces

    newIndex = index + 1
    comma = not (null rest)
    space = not (null rest) && not complexChildren
    newline = complexChildren

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
      forceComplexNewLine rs cursor
        || any (liftA2 (||) isSinglelineComment isComplexNode) nodes
          && not (noComplexNewLine rs cursor)

formatComment :: InternalComment -> Text
formatComment (InternalComment {cMultiline = False, cText = c}) = "// " <> c
formatComment (InternalComment {cMultiline = True, cText = c}) =
  "/*"
    <> leadingSpace
    <> c
    <> trailingSpace
    <> "*/"
  where
    leadingSpace = bool " " "" (T.isPrefixOf "\n" c)
    trailingSpace = bool " " "" (T.isSuffixOf "\n" c)

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
formatWithCursor rs cursor (ObjectKey (k, v)) = T.concat [formatWithCursor rs cursor k, " : ", formatWithCursor rs cursor v]
formatWithCursor _ _ (Comment comment) = formatComment comment
formatWithCursor rs cursor n =
  let ps = findPropertiesForCursor cursor rs
   in applyPadLogic formatScalarNode ps n

formatNode :: RuleSet -> Node -> Text
formatNode rs node = formatWithCursor rs newCursor node <> T.singleton '\n'

#ifdef ENABLE_WINDOWS_NEWLINES
replaceNewlines :: Text -> Text
replaceNewlines = T.replace "\n" "\r\n"
#else
replaceNewlines :: Text -> Text
replaceNewlines = id
#endif

formatNodeAndWrite
  :: RuleSet
  -> OsPath
  -> Node
  -> IO ()
formatNodeAndWrite rs outFile =
  OS.writeFile outFile
    . LBS.fromStrict
    . encodeUtf8
    . replaceNewlines
    . formatNode rs
