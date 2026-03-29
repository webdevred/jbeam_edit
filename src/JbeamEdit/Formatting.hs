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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid.Extra
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import Data.Vector qualified as V
import JbeamEdit.Core.Node (
  InternalComment (..),
  Node (..),
  NumberValue (..),
  expectArray,
  extractPreviousAssocCmt,
  isCommentNode,
  isComplexNode,
  isObjectKeyNode,
  isSinglelineComment,
  isStringNode,
  scientificToText,
 )
import JbeamEdit.Core.NodeCursor (newCursor)
import JbeamEdit.Core.NodeCursor qualified as NC
import JbeamEdit.Formatting.Rules (
  ComplexNewLineMode (..),
  MatchMode (..),
  PropertyKey (..),
  RuleSet (..),
  applyPadLogic,
  findPropertiesForCursor,
  lookupRule,
 )
import System.File.OsPath qualified as OS (writeFile)
import System.OsPath (OsPath)

data FormattingState = FormattingState
  { fsUsePad :: Bool
  , fsColumnWidths :: Vector Int
  , fsFormattedCache :: Vector (Vector Text)
  , fsHeaderWasExtracted :: Bool
  , fsCurrentRowIdx :: Maybe Int
  , fsObjectKeyWidth :: Maybe Int
  , fsSubObjectWidths :: Map Text Int
  }

emptyState :: FormattingState
emptyState =
  FormattingState
    { fsUsePad = False
    , fsColumnWidths = V.empty
    , fsFormattedCache = V.empty
    , fsHeaderWasExtracted = False
    , fsCurrentRowIdx = Nothing
    , fsObjectKeyWidth = Nothing
    , fsSubObjectWidths = Map.empty
    }

splitTrailing :: Bool -> Text -> (Text, Text)
splitTrailing comma txt =
  let trailing = T.length (T.takeWhileEnd (== ' ') txt)
      trailing' = trailing - bool 0 1 comma
   in ( T.dropEnd trailing txt
      , T.replicate trailing' " "
      )

normalizeCommentNode :: Bool -> Node -> Node
normalizeCommentNode False (Comment (InternalComment txt False dir hadNl)) = Comment (InternalComment txt True dir hadNl)
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
    newlineBeforeComment = case node of
      Comment ic | not (cMultiline ic) ->
        case acc of
          (prev : _)
            | T.isPrefixOf "// " (T.dropWhile (== '\n') prev) ->
                if cHadNewlineBefore ic then "\n" else ""
          _ ->
            singleCharIfNot
              '\n'
              (["\n"] == acc || not (cHadNewlineBefore ic) && any isObjectKeyNode rest)
      _ -> singleCharIfNot '\n' (any isObjectKeyNode rest || ["\n"] == acc)

    nextRowIdx =
      rowIdx + case node of
        Array _ -> 1
        _ -> 0

    applyCrumbAndFormat n idx =
      case fsCurrentRowIdx state of
        Just ri ->
          case fsFormattedCache state V.!? idx >>= (V.!? ri) of
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
            ObjectKey (k, _) ->
              let stateForObjKey =
                    emptyState
                      { fsObjectKeyWidth = fsObjectKeyWidth state
                      , fsSubObjectWidths = fsSubObjectWidths state
                      }
                  keyText = formatScalarNode False k
                  raw = NC.applyCrumb c (formatWithCursor rs stateForObjKey) idx n
                  (formatted, spaces) = splitTrailing comma raw
                  withComma = formatted <> singleCharIf ',' comma <> spaces
               in case Map.lookup keyText (fsSubObjectWidths state) of
                    Just vw
                      | comma ->
                          let kw = fromMaybe (T.length keyText) (fsObjectKeyWidth state)
                           in T.justifyLeft (kw + 3 + vw + 1) ' ' withComma
                    _ -> withComma
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
       in (columnWidths, formattedColumns, headerWasExtracted)
  where
    scalarLength n
      | T.any (== '\n') n = 0
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
transposeAndFormat rs cursor vvs arrayIndices
  | V.null vvs = V.empty
  | otherwise =
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
  let prefixProps = findPropertiesForCursor PrefixMatch cursor rs
      exactProps = findPropertiesForCursor ExactMatch cursor rs

      autoPadEnabled = lookupRule AutoPad exactProps == Just True
      alignObjectKeysEnabled = lookupRule AlignObjectKeys exactProps == Just True
      autopadSubObjectsEnabled = lookupRule AutoPadSubObjects exactProps == Just True

      complexChildren =
        lookupRule ComplexNewLine prefixProps == Just Force
          || any (liftA2 (||) isSinglelineComment isComplexNode) nodes
            && lookupRule ComplexNewLine prefixProps /= Just None

      (colWidths, formattedCache, headerWasExtracted) =
        maxColumnLengthsWithCache rs cursor nodes

      maxKeyW
        | not alignObjectKeysEnabled = Nothing
        | otherwise =
            let toKey (ObjectKey (k, _)) = Just k
                toKey _ = Nothing
                ks = V.mapMaybe toKey nodes
             in if V.null ks
                  then Nothing
                  else Just (V.maximum (V.map (T.length . formatScalarNode False) ks))

      subObjectWidths :: Map Text Int
      subObjectWidths
        | autopadSubObjectsEnabled =
            V.foldl'
              ( \acc n -> case n of
                  ObjectKey (_, Object subNodes) ->
                    V.foldl'
                      ( \acc2 sn -> case sn of
                          ObjectKey (sk, sv) ->
                            let kt = formatScalarNode False sk
                                vw =
                                  if isComplexNode sv
                                    then 0
                                    else
                                      let vt = formatWithCursor rs emptyState cursor sv
                                       in if T.any (== '\n') vt then 0 else T.length vt
                             in Map.insertWith max kt vw acc2
                          _ -> acc2
                      )
                      acc
                      subNodes
                  _ -> acc
              )
              Map.empty
              nodes
        | otherwise = Map.empty

      state'
        | autoPadEnabled
        , not (V.null colWidths) =
            FormattingState
              { fsUsePad = True
              , fsColumnWidths = colWidths
              , fsFormattedCache = formattedCache
              , fsHeaderWasExtracted = headerWasExtracted
              , fsCurrentRowIdx = Nothing
              , fsObjectKeyWidth = Nothing
              , fsSubObjectWidths = Map.empty
              }
        | alignObjectKeysEnabled || autopadSubObjectsEnabled =
            emptyState
              { fsObjectKeyWidth = maxKeyW
              , fsSubObjectWidths = subObjectWidths
              }
        | otherwise = state

      formatted =
        reverse
          . addDelimiters rs 0 0 cursor complexChildren state' []
          . V.toList
          $ nodes

      indentationAmount = fromMaybe 2 (lookupRule Indent prefixProps)
   in if complexChildren
        then
          T.unlines
            . map (applyIndentation indentationAmount)
            . concatMap T.lines
            $ formatted
        else T.concat formatted

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

formatScalarNode :: Bool -> Node -> Text
formatScalarNode _ (String s) = T.concat ["\"", s, "\""]
formatScalarNode True (Number nv) = nvText nv
formatScalarNode _ (Number nv) = scientificToText (nvValue nv)
formatScalarNode _ (Bool True) = "true"
formatScalarNode _ (Bool _) = "false"
formatScalarNode _ Null = "null"
formatScalarNode _ n = error $ "Unhandled scalar node: " <> show n

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
  let keyText = formatWithCursor rs state cursor k
      paddedKey = maybe keyText (\w -> T.justifyLeft w ' ' keyText) (fsObjectKeyWidth state)
      stateForValue = emptyState {fsSubObjectWidths = fsSubObjectWidths state}
      valueText = formatWithCursor rs stateForValue cursor v
   in paddedKey <> " : " <> valueText
formatWithCursor _ _ _ (Comment comment) = formatComment comment
formatWithCursor rs _ cursor n =
  let ps = findPropertiesForCursor PrefixMatch cursor rs
      preserve = (Just True == lookupRule PreserveNumberFormat ps)
   in applyPadLogic (formatScalarNode preserve) ps n

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
