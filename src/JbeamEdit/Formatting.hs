{-# LANGUAGE CPP #-}

module JbeamEdit.Formatting (
  formatNode,
  formatWithCursor,
  formatScalarNode,
  formatNodeAndWrite,
  RuleSet (..),
  FormattingState (..),
  emptyState,
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

data FormattingState = FormattingState
  { fsUsePad :: Bool
  , fsColumnWidths :: Vector Int
  , fsFormattedCache :: Vector (Vector Text)
  , fsHeaderWasExtracted :: Bool
  , fsCurrentRowIdx :: Maybe Int
  }

emptyState :: FormattingState
emptyState =
  FormattingState
    { fsUsePad = False
    , fsColumnWidths = V.empty
    , fsFormattedCache = V.empty
    , fsHeaderWasExtracted = False
    , fsCurrentRowIdx = Nothing
    }

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
addDelimiters rs index rowIdx c complexChildren state acc ns@(node : rest)
  | complexChildren && null acc =
      addDelimiters rs index rowIdx c complexChildren state ["\n"] ns
  | isCommentNode node =
      let formattedComment =
            formatWithCursor
              rs
              state
              c
              (normalizeCommentNode complexChildren node)
          formatted = (newlineBeforeComment <> formattedComment <> "\n") : acc
       in addDelimiters rs index rowIdx c complexChildren state formatted rest
  | otherwise =
      case extractPreviousAssocCmt rest of
        (Just comment, rest') ->
          let baseTxt = applyCrumbAndFormat node index
              formatted = (baseTxt <> " " <> formatComment comment) : acc
           in addDelimiters
                rs
                (index + 1)
                nextRowIdx
                c
                complexChildren
                state
                formatted
                rest'
        (Nothing, _) ->
          let baseTxt = applyCrumbAndFormat node index
              new_acc = (baseTxt <> singleCharIf ' ' space <> singleCharIf '\n' newline) : acc
           in addDelimiters rs (index + 1) nextRowIdx c complexChildren state new_acc rest
  where
    newlineBeforeComment = singleCharIfNot '\n' (any isObjectKeyNode rest || ["\n"] == acc)

    nextRowIdx =
      rowIdx + case node of
        Array _ -> 1
        _ -> 0

    applyCrumbAndFormat n idx =
      case fsCurrentRowIdx state of
        Just ri ->
          case fsFormattedCache state V.!? ri >>= (V.!? idx) of
            Just cellTxt ->
              let width = sum (fsColumnWidths state V.!? idx)
                  (stripped, spaces) = splitTrailing comma cellTxt
                  withComma = stripped <> singleCharIf ',' comma <> spaces
               in if comma then T.justifyLeft (width + 1) ' ' withComma else withComma
            Nothing ->
              let width = sum (fsColumnWidths state V.!? idx)
                  (stripped, spaces) = splitTrailing comma (NC.applyCrumb c (formatWithCursor rs emptyState) idx n)
                  withComma = stripped <> singleCharIf ',' comma <> spaces
               in if comma then T.justifyLeft (width + 1) ' ' withComma else withComma
        Nothing ->
          case n of
            Array _ ->
              let cacheIdx = rowIdx - fromEnum (fsHeaderWasExtracted state)
                  state' =
                    if fsUsePad state
                      then state {fsCurrentRowIdx = Just cacheIdx}
                      else emptyState
                  (formatted, spaces) = splitTrailing comma (NC.applyCrumb c (formatWithCursor rs state') idx n)
               in formatted <> singleCharIf ',' comma <> spaces
            _ ->
              let (formatted, spaces) = splitTrailing comma (NC.applyCrumb c (formatWithCursor rs emptyState) idx n)
               in formatted <> singleCharIf ',' comma <> spaces

    comma = notNull rest
    space = notNull rest && not complexChildren
    newline = complexChildren

applyIndentation :: Int -> Text -> Text
applyIndentation n s
  | T.all isSpace s = s
  | otherwise = T.replicate n " " <> s

maxColumnLengthsWithCache
  :: RuleSet
  -> NC.NodeCursor
  -> Vector Node
  -> (Vector Int, Vector (Vector Text), Bool)
maxColumnLengthsWithCache rs cursor nodes
  | V.null nodes = (V.empty, V.empty, False)
  | otherwise =
      let (nodesToProcess, headerWasExtracted) = case V.uncons nodes of
            Just (Array firstRow, rest) | all isStringNode firstRow -> (rest, True)
            _ -> (nodes, False)
          (arrayRows, arrayIndices) = extractArrayRows nodesToProcess (fromEnum headerWasExtracted)
          formattedColumns = transposeAndFormat rs cursor arrayRows arrayIndices
          columnWidths = V.map (V.maximum . V.map scalarLength) formattedColumns
          numDataRows = V.length arrayRows
          formattedRows = V.generate numDataRows $ \ri -> V.map (V.! ri) formattedColumns
       in (columnWidths, formattedRows, headerWasExtracted)
  where
    scalarLength n
      | T.isPrefixOf "{" n || T.isPrefixOf "[" n = 0
      | otherwise = T.length n
    extractArrayRows ns offset =
      let indexed = V.indexed ns
          arrays =
            V.mapMaybe
              ( \(idx, node) -> case expectArray node of
                  Just arr -> Just (arr, idx + offset)
                  Nothing -> Nothing
              )
              indexed
       in (V.map fst arrays, V.map snd arrays)

transposeAndFormat
  :: RuleSet
  -> NC.NodeCursor
  -> Vector (Vector Node)
  -> Vector Int
  -> Vector (Vector Text)
transposeAndFormat rs cursor vvs arrayIndices =
  let numCols = V.maximum (V.map V.length vvs)
   in V.generate numCols $ \colIdx ->
        V.imap
          ( \rowIdx row ->
              if colIdx < V.length row
                then
                  let actualRowIdx = arrayIndices V.! rowIdx
                      formatRow rowCursor _rowNode =
                        let formatCell = formatWithCursor rs emptyState
                         in NC.applyCrumb rowCursor formatCell colIdx (row V.! colIdx)
                   in NC.applyCrumb cursor formatRow actualRowIdx (Array row)
                else ""
          )
          vvs

doFormatNode
  :: RuleSet
  -> NC.NodeCursor
  -> FormattingState
  -> Vector Node
  -> Text
doFormatNode rs cursor state nodes =
  let autoPadEnabled =
        lookupPropertyForCursor ExactMatch AutoPad rs cursor == Just True

      (colWidths, formattedCache, headerWasExtracted) =
        maxColumnLengthsWithCache rs cursor nodes

      state' =
        if autoPadEnabled
          then
            FormattingState
              { fsUsePad = True
              , fsColumnWidths = colWidths
              , fsFormattedCache = formattedCache
              , fsHeaderWasExtracted = headerWasExtracted
              , fsCurrentRowIdx = Nothing
              }
          else state

      formatted =
        reverse
          . addDelimiters rs 0 0 cursor complexChildren state' []
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
formatWithCursor rs state cursor (Array a)
  | V.null a = "[]"
  | otherwise =
      T.concat
        [ "["
        , doFormatNode rs cursor state a
        , "]"
        ]
formatWithCursor rs state cursor (Object o)
  | V.null o = "{}"
  | otherwise =
      T.concat
        [ "{"
        , doFormatNode rs cursor state o
        , "}"
        ]
formatWithCursor rs state cursor (ObjectKey (k, v)) =
  T.concat
    [ formatWithCursor rs state cursor k
    , " : "
    , formatWithCursor rs state cursor v
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
