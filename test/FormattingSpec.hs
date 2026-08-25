module FormattingSpec (
  spec,
) where

import Control.Monad (forM, forM_)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.IsList (fromList)
import JbeamEdit.Core.NodeCursor (newCursor)
import JbeamEdit.Formatting
import SpecHelper
import System.FilePath (takeBaseName, (</>))

numberSpec :: [(String, Node)]
numberSpec =
  [ ("123", Number (mkNumberValue "123" 123))
  , ("123.123", Number (mkNumberValue "123.123" 123.123))
  , ("-123", Number (mkNumberValue "-123" (-123)))
  , ("-123.123", Number (mkNumberValue "-123.123" (-123.123)))
  , ("0", Number (mkNumberValue "0" 0))
  ]

stringSpec :: [(String, Node)]
stringSpec = [("\"test\"", String "test"), ("\"\"", String "")]

boolSpec :: [(String, Node)]
boolSpec = [("true", Bool True), ("false", Bool False)]

nullSpec :: [(String, Node)]
nullSpec = [("null", Null)]

multilineCommentSpec :: [(String, Node)]
multilineCommentSpec = [("/* test */", Comment (InternalComment "test" True NextNode False))]

singlelineCommentSpec :: [(String, Node)]
singlelineCommentSpec = [("// test", Comment (InternalComment "test" False NextNode False))]

arraySpec :: [(String, Node)]
arraySpec =
  [
    ( "[1, 2, 3]"
    , mkArray
        ( fromList
            [ Number (mkNumberValue "1" 1)
            , Number (mkNumberValue "2" 2)
            , Number (mkNumberValue "3" 3)
            ]
        )
    )
  ]

objectSpec :: [(String, Node)]
objectSpec =
  [
    ( "{\"test\" : 1, \"test2\" : 2}"
    , mkObject
        ( fromList
            [ ObjectKey (String "test", Number (mkNumberValue "1" 1))
            , ObjectKey (String "test2", Number (mkNumberValue "2" 2))
            ]
        )
    )
  ]

dynamicJbflTests :: IO [(FilePath, Text, Text)]
dynamicJbflTests = do
  let examplesDir = "examples"
      jbeamAstDir = examplesDir </> "ast/jbeam"
      jbflAstDir = examplesDir </> "ast/jbfl"
      formattedDir = examplesDir </> "formatted_jbeam"

  jbeamFiles <- listFilesInDir jbeamAstDir
  jbflFiles <- listFilesInDir jbflAstDir

  forM [(j, b) | j <- jbeamFiles, b <- jbflFiles] $ \(jbeamFile, jbflFile) -> do
    jbeam <- read <$> readFile (jbeamAstDir </> jbeamFile)
    rules <- read <$> readFile (jbflAstDir </> jbflFile)

    let formatted = formatNode rules jbeam
        baseName = takeBaseName jbeamFile ++ "-" ++ takeBaseName jbflFile
        outFile = formattedDir </> (baseName ++ "-jbfl.jbeam")

    expected <- T.pack <$> readFile outFile
    pure (outFile, formatted, expected)

reachSpec :: Spec
reachSpec = do
  let row cells = mkArray (fromList cells)
      rows =
        row
          [ row [String "a_long_name", Number (mkNumberValue "1" 1)]
          , row [String "n1", Number (mkNumberValue "2" 2)]
          ]
      topNode =
        mkObject
          ( fromList
              [ ObjectKey
                  ( String "part"
                  , mkObject (fromList [ObjectKey (String "rows", rows)])
                  )
              ]
          )
      formatWith src = formatNode (rulesFromSource src) topNode
      shortPattern prop = ".* { " <> prop <> " }"
      exactPattern prop = ".*.rows { " <> prop <> " }"

      wrap body = "{\"part\" : {\n    \"rows\" : [\n" <> body <> "\n    ]\n}}\n"
      baseline = wrap "        [\"a_long_name\", 1],\n        [\"n1\", 2]"
      padded = wrap "        [\"a_long_name\", 1],\n        [\"n1\",          2]"

  describe "how far down a property reaches" $ do
    it "applies AutoPad to the matched value only" $ do
      formatWith (exactPattern "AutoPad : true;") `shouldBe` padded
      -- Guards the line below: a shortPattern matching nothing passes it too.
      formatWith (shortPattern "ComplexNewLine : Force;") `shouldNotBe` baseline
      formatWith (shortPattern "AutoPad : true;") `shouldBe` baseline

    it "applies ComplexNewLine below the matched value too" $
      formatWith (shortPattern "ComplexNewLine : Force;") `shouldNotBe` baseline

{- | A single cell in a `nodes` row, formatted with the ruleset given. The
padding properties only reach values under `nodes`, so the cell needs the
structure around it even when the spec is about one number.
-}
paddedCell :: String -> Node -> Text
paddedCell rules cell =
  formatNode (rulesFromSource rules) (docWith cell)
  where
    row cells = mkArray (fromList cells)
    docWith c =
      mkObject
        ( fromList
            [ ObjectKey
                ( String "part"
                , mkObject (fromList [ObjectKey (String "nodes", row [row [c]])])
                )
            ]
        )

-- | The one-line document `paddedCell` produces, around an already formatted cell.
cellOutput :: Text -> Text
cellOutput body = "{\"part\" : {\"nodes\" : [[" <> body <> "]]}}\n"

{- | `PadDecimals` guarantees a minimum number of decimal digits, so a
coordinate the source wrote as `12.0` should come out with three of them. It
comes out as `12`, because `scientificToText` rebuilds the text from the
parsed value and drops the point for a whole number, and `applyDecimalPadding`
only pads text that already contains one. The source text is still on the node
in `nvText`, so nothing is lost at parse time. Issue #217.

The other two cases are controls: padding still works where the source has
decimals, and a source that wrote no point is left alone, because padding it
would change what the author meant.
-}
decimalPaddingSpec :: Spec
decimalPaddingSpec = do
  let formatCell = paddedCell ".*.nodes[*][*] { PadDecimals: 3; }"
      wrap = cellOutput

  describe "PadDecimals" $ do
    it "pads a whole number the source wrote with a decimal point" $
      formatCell (Number (mkNumberValue "12.0" 12)) `shouldBe` wrap "12.000"

    it "pads one that already has decimals" $
      formatCell (Number (mkNumberValue "1.2" 1.2)) `shouldBe` wrap "1.200"

    it "leaves one written without a decimal point alone" $
      formatCell (Number (mkNumberValue "12" 12)) `shouldBe` wrap "12"

{- | `JBFL_DOCS.md` gives a table for `PadDecimals: 3` with `PadAmount: 8`:
1.2 becomes 1.200000 and 3.14 becomes 3.140000, described as "padding with
trailing zeros after the decimal point". Both come out padded with spaces
instead, so the number keeps three decimals and the row is widened with
blanks. Either the table or the code is wrong; these specs pick the table,
because that is what a reader configures against. Issue #217.
-}
padAmountSpec :: Spec
padAmountSpec = do
  let formatCell = paddedCell ".*.nodes[*][*] { PadDecimals: 3; PadAmount: 8; }"
      wrap = cellOutput

  describe "PadAmount" $ do
    it "fills a number that has decimals with trailing zeros" $ do
      formatCell (Number (mkNumberValue "1.2" 1.2)) `shouldBe` wrap "1.200000"
      formatCell (Number (mkNumberValue "3.14" 3.14)) `shouldBe` wrap "3.140000"

    it "fills a number written without a point with spaces" $
      formatCell (Number (mkNumberValue "12" 12)) `shouldBe` wrap "12      "

spec :: Spec
spec = do
  mapM_ formatNodeSpec specs
  reachSpec
  decimalPaddingSpec
  padAmountSpec

  dynamicTests <- runIO dynamicJbflTests
  forM_ dynamicTests $ \(outFile, formatted, expected) ->
    it ("formats JBEAM AST to " ++ outFile) $
      shouldBe formatted expected
  where
    formatNodeSpec (jbeam, node) =
      applySpecOnInput
        descFun
        shouldBe
        (formatWithCursor mempty emptyState newCursor node)
        (T.pack jbeam)
    descFun jbeam node = "should format " ++ show node ++ " as " ++ jbeam
    specs =
      concat
        [ numberSpec
        , stringSpec
        , boolSpec
        , nullSpec
        , multilineCommentSpec
        , singlelineCommentSpec
        , arraySpec
        , objectSpec
        ]
