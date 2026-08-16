module FormattingSpec (
  spec,
) where

import Control.Monad (forM, forM_)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.IsList (fromList)
import JbeamEdit.Core.NodeCursor (newCursor)
import JbeamEdit.Core.NodePath qualified as NP
import JbeamEdit.Formatting
import JbeamEdit.Formatting.Rules
import JbeamEdit.Formatting.Rules.ComplexNewLine qualified as CNL
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

{- | Which mode a property is read in is hardcoded in `doFormatNode`: AutoPad,
AlignObjectKeys and AutoPadSubObjects come from an exact match, ComplexNewLine
and TrailingComma from a prefix match. Moving one across changes formatting and
no fixture notices. This pins the split as it stands before `>` (see #187), which
is meant to replace it, so expect to rewrite this when that lands.
-}
matchModeSpec :: Spec
matchModeSpec = do
  let row cells = mkArray (fromList cells)
      -- The first column has to vary in width for AutoPad to show, since
      -- trailing spaces on the last one are trimmed either way.
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
      -- The rows array sits two breadcrumbs deep, so a one-selector pattern is
      -- a prefix of its cursor and a two-selector one matches it exactly.
      ruleAt p k v =
        RuleSet
          (fromList [(NodePattern (fromList p), fromList [(SomeKey k, SomeProperty k v)])])
      shortPattern = [AnyObjectKey]
      exactPattern = [AnyObjectKey, Selector (NP.ObjectKey "rows")]
      formatWith rs = formatNode rs topNode

      -- The only difference is the run of spaces before the 2, which is the
      -- second column padded out to the width of the first row.
      wrap body = "{\"part\" : {\n    \"rows\" : [\n" <> body <> "\n    ]\n}}\n"
      baseline = wrap "        [\"a_long_name\", 1],\n        [\"n1\", 2]"
      padded = wrap "        [\"a_long_name\", 1],\n        [\"n1\",          2]"

  describe "which match mode a property is read in" $ do
    it "reads AutoPad from an exact match only" $ do
      formatWith (ruleAt exactPattern AutoPad True) `shouldBe` padded
      -- Without this line the assertion below also passes for a shortPattern
      -- that matches nothing at all, which is not what is being claimed.
      formatWith (ruleAt shortPattern ComplexNewLine CNL.Force)
        `shouldNotBe` baseline
      formatWith (ruleAt shortPattern AutoPad True) `shouldBe` baseline

    it "reads ComplexNewLine from a prefix match" $
      formatWith (ruleAt shortPattern ComplexNewLine CNL.Force)
        `shouldNotBe` baseline

spec :: Spec
spec = do
  mapM_ formatNodeSpec specs
  matchModeSpec

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
