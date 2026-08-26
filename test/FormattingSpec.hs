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

{- | The padding properties only reach values under `nodes`, so a spec about one
number still has to build the structure around it.
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

cellOutput :: Text -> Text
cellOutput body = "{\"part\" : {\"nodes\" : [[" <> body <> "]]}}\n"

{- | `12.0` comes out as `12`: `scientificToText` rebuilds the text from the
parsed value and drops the point, and `applyDecimalPadding` only pads text that
already has one. The source text is still on the node in `nvText`, so the fix
belongs in the formatter and not in the parser. Issue #217.
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

    it "trims trailing zeros back to the minimum" $
      formatCell (Number (mkNumberValue "0.12000" 0.12)) `shouldBe` wrap "0.120"

    it "leaves significant decimals past the minimum alone" $
      formatCell (Number (mkNumberValue "0.12345" 0.12345)) `shouldBe` wrap "0.12345"

{- | `JBFL_DOCS.md` described this twice and got it wrong both times, once as
trailing zeros and once as leading spaces, so a reader could reasonably try to
make either true. The shipped `complex.jbfl` depends on the real behaviour to
line up its `glowMap` columns.
-}
padAmountSpec :: Spec
padAmountSpec = do
  let formatCell = paddedCell ".*.nodes[*][*] { PadAmount: 8; }"
      wrap = cellOutput

  describe "PadAmount"
    . it "fills the value out to the width with trailing spaces"
    $ do
      formatCell (Number (mkNumberValue "7.89" 7.89)) `shouldBe` wrap "7.89    "
      formatCell (Number (mkNumberValue "0.1234" 0.1234)) `shouldBe` wrap "0.1234  "
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
