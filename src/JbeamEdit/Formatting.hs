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
import Data.Foldable.Extra (notNull)
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Monoid.Extra
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB (toLazyText)
import Data.Text.Lazy.Builder.Scientific
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Vector (Vector)
import Data.Vector qualified as V
import JbeamEdit.Core.Node (
  InternalComment (..),
  Node (..),
  expectArray,
  extractPreviousAssocCmt,
  isCommentNode,
  isComplexNode,
  isObjectKeyNode,
  isSinglelineComment,
  isStringNode,
 )
import JbeamEdit.Core.NodeCursor (newCursor)
import JbeamEdit.Core.NodeCursor qualified as NC
import JbeamEdit.Formatting.Rules (
  MatchMode (..),
  PropertyKey (..),
  RuleSet (..),
  applyPadLogic,
  findPropertiesForCursor,
  forceComplexNewLine,
  lookupPropertyForCursor,
  noComplexNewLine,
 )
import System.File.OsPath qualified as OS (writeFile)
import System.OsPath (OsPath)

splitTrailing :: Bool -> TL.Text -> (TL.Text, TL.Text)
splitTrailing comma txt =
  let trailing = TL.length (TL.takeWhileEnd (== ' ') txt)
      trailing' = trailing - bool 0 1 comma
   in ( TL.dropEnd trailing txt
      , TL.replicate trailing' " "
      )

normalizeCommentNode :: Bool -> Node -> Node
normalizeCommentNode False (Comment (InternalComment txt False dir)) = Comment (InternalComment txt True dir)
normalizeCommentNode _ node = node

singleCharIf :: Char -> Bool -> TL.Text
singleCharIf a b = mwhen b (TL.singleton a)

singleCharIfNot :: Char -> Bool -> TL.Text
singleCharIfNot a b = singleCharIf a (not b)

addDelimiters
  :: RuleSet
  -> Int
  -> NC.NodeCursor
  -> Bool
  -> (Bool, Vector Int64) -- (usePad, columnWidths)
  -> [TL.Text]
  -> [Node]
  -> [TL.Text]
addDelimiters _ _ _ _ _ acc [] = acc
addDelimiters rs index c complexChildren (usePad, colWidths) acc ns@(node : rest)
  | complexChildren && null acc =
      addDelimiters rs index c complexChildren (usePad, colWidths) ["\n"] ns
  | isCommentNode node =
      let formattedComment =
            formatWithCursor
              rs
              (usePad, colWidths)
              c
              (normalizeCommentNode complexChildren node)
          formatted = (newlineBeforeComment <> formattedComment <> "\n") : acc
       in addDelimiters rs index c complexChildren (usePad, colWidths) formatted rest
  | otherwise =
      case extractPreviousAssocCmt rest of
        (Just comment, rest') ->
          let baseTxt = applyCrumbAndFormat node
              formatted = (padTxt baseTxt <> " " <> formatComment comment) : acc
           in addDelimiters
                rs
                (index + 1)
                c
                complexChildren
                (usePad, colWidths)
                formatted
                rest'
        (Nothing, _) ->
          let baseTxt = applyCrumbAndFormat node
              new_acc = (padTxt baseTxt <> singleCharIf ' ' space <> singleCharIf '\n' newline) : acc
           in addDelimiters rs (index + 1) c complexChildren (usePad, colWidths) new_acc rest
  where
    newlineBeforeComment = singleCharIfNot '\n' (any isObjectKeyNode rest || ["\n"] == acc)

    applyCrumbAndFormat n =
      let padded = NC.applyCrumb c (formatWithCursor rs (usePad, colWidths)) index n
          (formatted, spaces) = splitTrailing comma padded
       in formatted <> singleCharIf ',' comma <> spaces

    padTxt baseTxt =
      if usePad && not (isCommentNode node) && comma
        then
          let width = sum (colWidths V.!? index)
           in TL.justifyLeft (width + 1) ' ' baseTxt
        else baseTxt

    comma = notNull rest
    space = notNull rest && not complexChildren
    newline = complexChildren

applyIndentation :: Int64 -> TL.Text -> TL.Text
applyIndentation n s
  | TL.all isSpace s = s
  | otherwise = TL.replicate n " " <> s

skipHeaderRow :: Vector (Vector Node) -> Vector (Vector Node)
skipHeaderRow nodes
  | V.length nodes > 1 =
      bool nodes (V.unsafeTail nodes) (all isStringNode $ V.unsafeHead nodes)
  | otherwise = nodes

maxColumnLengths
  :: RuleSet -> NC.NodeCursor -> Vector (Vector Node) -> Vector Int64
maxColumnLengths rs cursor rows
  | V.null rows = V.empty
  | otherwise =
      V.map
        (V.maximum . V.map TL.length)
        (transposeWithPadding rs cursor $ skipHeaderRow rows)

transposeWithPadding
  :: RuleSet -> NC.NodeCursor -> Vector (Vector Node) -> Vector (Vector TL.Text)
transposeWithPadding rs cursor vvs =
  let numCols = V.maximum (V.map V.length vvs)
   in V.generate numCols $ \j ->
        V.map
          ( \row ->
              mwhen
                (j < V.length row)
                (formatWithCursor rs (False, V.empty) cursor (row V.! j))
          )
          vvs

doFormatNode
  :: RuleSet
  -> NC.NodeCursor
  -> (Bool, Vector Int64)
  -> Vector Node
  -> TL.Text
doFormatNode rs cursor padAmounts nodes =
  let autoPadEnabled =
        lookupPropertyForCursor ExactMatch AutoPad rs cursor == Just True

      childrenVectors = V.map (fromMaybe V.empty . expectArray) nodes
      padAmounts' = maxColumnLengths rs cursor childrenVectors
      maybePadAmounts =
        bool padAmounts (False, padAmounts') autoPadEnabled

      formatted =
        reverse
          . addDelimiters rs 0 cursor complexChildren maybePadAmounts []
          . V.toList
          $ nodes

      indentationAmount =
        fromMaybe 2 (lookupPropertyForCursor PrefixMatch Indent rs cursor)
   in if complexChildren
        then
          TL.unlines
            . map (applyIndentation indentationAmount)
            . concatMap TL.lines
            $ formatted
        else TL.concat formatted
  where
    complexChildren =
      forceComplexNewLine rs cursor
        || any (liftA2 (||) isSinglelineComment isComplexNode) nodes
          && not (noComplexNewLine rs cursor)

formatComment :: InternalComment -> TL.Text
formatComment (InternalComment {cMultiline = False, cText = c}) = "// " <> TL.fromStrict c
formatComment (InternalComment {cMultiline = True, cText = c}) =
  "/*"
    <> leadingSpace
    <> TL.fromStrict c
    <> trailingSpace
    <> "*/"
  where
    leadingSpace = singleCharIfNot ' ' (T.isPrefixOf "\n" c)
    trailingSpace = singleCharIfNot ' ' (T.isSuffixOf "\n" c)

formatScalarNode :: Node -> TL.Text
formatScalarNode (String s) = "\"" <> TL.fromStrict s <> "\""
formatScalarNode (Number n) = TLB.toLazyText $ formatScientificBuilder Fixed Nothing n
formatScalarNode (Bool True) = "true"
formatScalarNode (Bool _) = "false"
formatScalarNode Null = "null"
formatScalarNode _ = error "Unhandled scalar node"

formatWithCursor
  :: RuleSet -> (Bool, Vector Int64) -> NC.NodeCursor -> Node -> TL.Text
formatWithCursor rs (_, maybePadAmounts) cursor (Array a)
  | V.null a = "[]"
  | otherwise =
      "["
        <> doFormatNode rs cursor (notNull maybePadAmounts, maybePadAmounts) a
        <> "]"
formatWithCursor rs (_, maybePadAmounts) cursor (Object o)
  | V.null o = "{}"
  | otherwise =
      "{"
        <> doFormatNode rs cursor (notNull maybePadAmounts, maybePadAmounts) o
        <> "}"
formatWithCursor rs (_, maybePadAmounts) cursor (ObjectKey (k, v)) =
  formatWithCursor rs (notNull maybePadAmounts, maybePadAmounts) cursor k
    <> " : "
    <> formatWithCursor rs (notNull maybePadAmounts, maybePadAmounts) cursor v
formatWithCursor _ _ _ (Comment comment) = formatComment comment
formatWithCursor rs _ cursor n =
  let ps = findPropertiesForCursor PrefixMatch cursor rs
   in applyPadLogic formatScalarNode ps n

formatNode :: RuleSet -> Node -> TL.Text
formatNode rs node = formatWithCursor rs (False, V.empty) newCursor node <> TL.singleton '\n'

#ifdef ENABLE_WINDOWS_NEWLINES
replaceNewlines :: TL.Text -> TL.Text
replaceNewlines = T.replace "\n" "\r\n"
#else
replaceNewlines :: TL.Text -> TL.Text
replaceNewlines = id
#endif

formatNodeAndWrite
  :: RuleSet
  -> OsPath
  -> Node
  -> IO ()
formatNodeAndWrite rs outFile =
  OS.writeFile outFile
    . encodeUtf8
    . replaceNewlines
    . formatNode rs
