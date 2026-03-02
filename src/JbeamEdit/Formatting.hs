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
import Data.Maybe (fromMaybe)
import Data.Monoid.Extra
import Data.Scientific (FPFormat (Fixed), formatScientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
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
singleCharIf a b = mwhen b (T.singleton a)

singleCharIfNot :: Char -> Bool -> Text
singleCharIfNot a b = singleCharIf a (not b)

data FormattingState = FormattingState
  { fsUsePad :: Bool
  , fsColumnWidths :: UV.Vector Int
  , fsFormattedCache :: Vector (Vector Text)
  , fsHeaderCache :: Maybe (Vector Text)
  , fsHeaderWasExtracted :: Bool
  }

emptyState :: FormattingState
emptyState = FormattingState False UV.empty V.empty Nothing False

addDelimiters
  :: RuleSet
  -> Int
  -> Int
  -> NC.NodeCursor
  -> Bool
  -> FormattingState
  -> [Text]
  -> [Node]
  -> [Text]
addDelimiters _ _ _ _ _ _ acc [] = acc
addDelimiters rs rowIndex index c complexChildren fs acc ns@(node : rest)
  | complexChildren && null acc =
      addDelimiters rs rowIndex index c complexChildren fs ["\n"] ns
  | isCommentNode node =
      let formattedComment =
            formatWithCursor rs fs c (normalizeCommentNode complexChildren node)
          formatted = (newlineBeforeComment <> formattedComment <> "\n") : acc
       in addDelimiters rs rowIndex index c complexChildren fs formatted rest
  | otherwise =
      case extractPreviousAssocCmt rest of
        (Just comment, rest') ->
          let baseTxt = applyCrumbAndFormat node
              formatted = (padTxt baseTxt <> " " <> formatComment comment) : acc
           in addDelimiters rs rowIndex (index + 1) c complexChildren fs formatted rest'
        (Nothing, _) ->
          let baseTxt = applyCrumbAndFormat node
              new_acc = (padTxt baseTxt <> singleCharIf ' ' space <> singleCharIf '\n' newline) : acc
           in addDelimiters rs rowIndex (index + 1) c complexChildren fs new_acc rest
  where
    newlineBeforeComment = singleCharIfNot '\n' (any isObjectKeyNode rest || ["\n"] == acc)

    applyCrumbAndFormat n =
      -- Try to use cache if available and valid
      let cachedText = lookupCache rowIndex index
          padded = case cachedText of
            Just txt -> txt
            Nothing -> NC.applyCrumb c (formatWithCursor rs fs) index n
          (formatted, spaces) = splitTrailing comma padded
       in formatted <> singleCharIf ',' comma <> spaces

    lookupCache rIdx cIdx =
      if fsUsePad fs && not (V.null (fsFormattedCache fs))
        then
          if fsHeaderWasExtracted fs && rIdx == 0
            then fsHeaderCache fs >>= (V.!? cIdx)
            else
              let dataRowIdx = if fsHeaderWasExtracted fs then rIdx - 1 else rIdx
               in if dataRowIdx >= 0 && dataRowIdx < V.length (fsFormattedCache fs)
                    then (fsFormattedCache fs V.! dataRowIdx) V.!? cIdx
                    else Nothing
        else Nothing

    padTxt baseTxt =
      if fsUsePad fs && not (isCommentNode node) && comma
        then
          let width = sum (fsColumnWidths fs UV.!? index)
           in T.justifyLeft (width + 1) ' ' baseTxt
        else baseTxt

    comma = notNull rest
    space = notNull rest && not complexChildren
    newline = complexChildren

applyIndentation :: Int -> Text -> Text
applyIndentation n s
  | T.all isSpace s = s
  | otherwise = T.replicate n " " <> s

extractHeader
  :: Vector (Vector Node) -> (Maybe (Vector Node), Vector (Vector Node))
extractHeader nodes =
  case V.uncons nodes of
    Just (headerRow, rest) ->
      if all isStringNode headerRow
        then (Just headerRow, rest)
        else (Nothing, nodes)
    Nothing -> (Nothing, nodes)

maxColumnLengthsWithCache
  :: RuleSet
  -> NC.NodeCursor
  -> Vector (Vector Node)
  -> (Maybe (Vector Text), UV.Vector Int, Vector (Vector Text), Bool)
maxColumnLengthsWithCache rs cursor rows
  | V.null rows = (Nothing, UV.empty, V.empty, False)
  | otherwise =
      let (mHeader, dataRows) = extractHeader rows
          headerWasExtracted = mHeader /= Nothing
          headerFormatted = V.map (NC.applyCrumb cursor (formatWithCursor rs emptyState) 0) <$> mHeader
          formatted = transposeWithPadding rs cursor dataRows
          colLengths =
            if V.null formatted
              then UV.empty
              else
                UV.fromList $
                  V.foldr (\col acc -> V.maximum (V.map T.length col) : acc) [] formatted
       in (headerFormatted, colLengths, formatted, headerWasExtracted)

transposeWithPadding
  :: RuleSet -> NC.NodeCursor -> Vector (Vector Node) -> Vector (Vector Text)
transposeWithPadding rs cursor vvs =
  let numCols = if V.null vvs then 0 else V.maximum (V.map V.length vvs)
   in if numCols == 0
        then V.empty
        else V.generate numCols $ \j ->
          V.imap
            ( \i row ->
                mwhen
                  (j < V.length row)
                  (NC.applyCrumb cursor (formatWithCursor rs emptyState) i (row V.! j))
            )
            vvs

doFormatNode
  :: RuleSet
  -> NC.NodeCursor
  -> FormattingState
  -> Vector Node
  -> Text
doFormatNode rs cursor _ nodes =
  let autoPadEnabled =
        lookupPropertyForCursor ExactMatch AutoPad rs cursor == Just True

      childrenVectors = V.map (fromMaybe V.empty . expectArray) nodes
      (mHeaderFormatted, colWidths, formattedCache, headerExtracted) =
        maxColumnLengthsWithCache rs cursor childrenVectors

      fs' =
        if autoPadEnabled
          then
            FormattingState True colWidths formattedCache mHeaderFormatted headerExtracted
          else emptyState

      formatted =
        reverse
          $ addDelimitersWithRowIndex rs 0 cursor complexChildren fs' [] (V.toList nodes)

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

    -- Wrapper that tracks row indices for array nodes
    addDelimitersWithRowIndex
      :: RuleSet
      -> Int
      -> NC.NodeCursor
      -> Bool
      -> FormattingState
      -> [Text]
      -> [Node]
      -> [Text]
    addDelimitersWithRowIndex _ _ _ _ _ acc [] = acc
    addDelimitersWithRowIndex rsLocal rowIdx cLocal complexChildrenLocal fsLocal acc (n : ns) =
      let result = addDelimiters rsLocal rowIdx 0 cLocal complexChildrenLocal fsLocal acc [n]
       in addDelimitersWithRowIndex rsLocal (rowIdx + 1) cLocal complexChildrenLocal fsLocal result ns

formatComment :: InternalComment -> Text
formatComment (InternalComment {cMultiline = False, cText = c}) = "// " <> c
formatComment (InternalComment {cMultiline = True, cText = c}) =
  "/*"
    <> leadingSpace
    <> c
    <> trailingSpace
    <> "*/"
  where
    leadingSpace = singleCharIfNot ' ' (T.isPrefixOf "\n" c)
    trailingSpace = singleCharIfNot ' ' (T.isSuffixOf "\n" c)

formatScalarNode :: Node -> Text
formatScalarNode (String s) = T.concat ["\"", s, "\""]
formatScalarNode (Number n) = T.pack (formatScientific Fixed Nothing n)
formatScalarNode (Bool True) = "true"
formatScalarNode (Bool _) = "false"
formatScalarNode Null = "null"
formatScalarNode _ = error "Unhandled scalar node"

formatWithCursor
  :: RuleSet -> FormattingState -> NC.NodeCursor -> Node -> Text
formatWithCursor rs _ cursor (Array a)
  | V.null a = "[]"
  | otherwise =
      T.concat
        [ "["
        , doFormatNode rs cursor emptyState a
        , "]"
        ]
formatWithCursor rs _ cursor (Object o)
  | V.null o = "{}"
  | otherwise =
      T.concat
        [ "{"
        , doFormatNode rs cursor emptyState o
        , "}"
        ]
formatWithCursor rs _ cursor (ObjectKey (k, v)) =
  T.concat
    [ formatWithCursor rs emptyState cursor k
    , " : "
    , formatWithCursor rs emptyState cursor v
    ]
formatWithCursor _ _ _ (Comment comment) = formatComment comment
formatWithCursor rs _ cursor n =
  let ps = findPropertiesForCursor PrefixMatch cursor rs
   in applyPadLogic formatScalarNode ps n

formatNode :: RuleSet -> Node -> Text
formatNode rs node = formatWithCursor rs emptyState newCursor node <> T.singleton '\n'

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
