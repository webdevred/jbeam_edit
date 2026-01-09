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
import Data.Maybe (fromMaybe)
import Data.Scientific (FPFormat (Fixed), formatScientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
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
  :: RuleSet
  -> Int
  -> NC.NodeCursor
  -> Bool
  -> (Bool, Vector Int) -- (usePad, columnWidths)
  -> [Text]
  -> [Node]
  -> [Text]
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
              paddedTxt =
                if usePad
                  then padText index baseTxt
                  else baseTxt
              formatted = (paddedTxt <> " " <> formatComment comment) : acc
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
              paddedTxt =
                if usePad
                  then padText index baseTxt
                  else baseTxt
              new_acc = (paddedTxt <> singleCharIf ' ' space <> singleCharIf '\n' newline) : acc
           in addDelimiters rs (index + 1) c complexChildren (usePad, colWidths) new_acc rest
  where
    newlineBeforeComment = bool "\n" "" $ any isObjectKeyNode rest || ["\n"] == acc

    applyCrumbAndFormat n =
      let padded = NC.applyCrumb c (formatWithCursor rs (usePad, colWidths)) index n
          (formatted, spaces) = splitTrailing comma padded
       in formatted <> singleCharIf ',' comma <> spaces

    padText i txt =
      let width = sum (colWidths V.!? i)
       in T.justifyLeft (bool width (width + 1) comma) ' ' txt

    comma = not (null rest)
    space = not (null rest) && not complexChildren
    newline = complexChildren

applyIndentation :: Int -> Text -> Text
applyIndentation n s
  | T.all isSpace s = s
  | otherwise = T.replicate n " " <> s

skipHeaderRow :: Vector (Vector  Node) -> Vector (Vector Node)
skipHeaderRow nodes =
  case V.uncons nodes of
    Just (headerRow, rest) ->
      bool nodes rest (all isStringNode headerRow)
    Nothing -> nodes

maxColumnLengths
  :: RuleSet -> NC.NodeCursor -> Vector (Vector Node) -> Vector Int
maxColumnLengths rs cursor rows
  | V.null rows = V.empty
  | otherwise =
      V.map
        (V.maximum . V.map T.length)
        (transposeWithPadding rs cursor $ skipHeaderRow rows)

transposeWithPadding
  :: RuleSet -> NC.NodeCursor -> Vector (Vector Node) -> Vector (Vector T.Text)
transposeWithPadding rs cursor vvs =
  let numCols = V.maximum (V.map V.length vvs)
   in V.generate numCols $ \j ->
        V.map
          ( \row ->
              bool
                ""
                (formatWithCursor rs (False, V.empty) cursor (row V.! j))
                (j < V.length row)
          )
          vvs

doFormatNode
  :: RuleSet
  -> NC.NodeCursor
  -> (Bool, Vector Int)
  -> Vector Node
  -> Text
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
          T.unlines
            . map (applyIndentation indentationAmount)
            . concatMap T.lines
            $ formatted
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

formatWithCursor
  :: RuleSet -> (Bool, Vector Int) -> NC.NodeCursor -> Node -> Text
formatWithCursor rs (_, maybePadAmounts) cursor (Array a)
  | V.null a = "[]"
  | otherwise =
      T.concat
        [ "["
        , doFormatNode rs cursor (not (V.null maybePadAmounts), maybePadAmounts) a
        , "]"
        ]
formatWithCursor rs (_, maybePadAmounts) cursor (Object o)
  | V.null o = "{}"
  | otherwise =
      T.concat
        [ "{"
        , doFormatNode rs cursor (not (V.null maybePadAmounts), maybePadAmounts) o
        , "}"
        ]
formatWithCursor rs (_, maybePadAmounts) cursor (ObjectKey (k, v)) =
  T.concat
    [ formatWithCursor rs (not (V.null maybePadAmounts), maybePadAmounts) cursor k
    , " : "
    , formatWithCursor rs (not (V.null maybePadAmounts), maybePadAmounts) cursor v
    ]
formatWithCursor _ _ _ (Comment comment) = formatComment comment
formatWithCursor rs _ cursor n =
  let ps = findPropertiesForCursor PrefixMatch cursor rs
   in applyPadLogic formatScalarNode ps n

formatNode :: RuleSet -> Node -> Text
formatNode rs node = formatWithCursor rs (False, V.empty) newCursor node <> T.singleton '\n'

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
