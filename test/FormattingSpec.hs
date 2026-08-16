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
import JbeamEdit.Parsing.DSL (parseDSL)
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

{- | Which mode a property is read in is hardcoded in the formatter. Three come
from an exact match, AutoPad, AlignObjectKeys and AutoPadSubObjects, and they
are the ones about how a container lays out its own children. The other six
cascade and come from a prefix match: ComplexNewLine, TrailingComma, Indent,
PreserveNumberFormat, PadAmount and PadDecimals.

Moving one across changes formatting and no fixture notices. This pins the split
as it stands before `>` (see #187), which is meant to replace it, so expect to
rewrite this when that lands.
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

{- | When several patterns match the same node, the more specific one supplies
the property. Specificity is how many nodes a selector can match at that level:
a named key first, then a positional index, then a prefix key with the longer
prefix winning, then the wildcards.

Written as JBFL source and formatted output on purpose. Both ends survive the
rule lookup becoming a trie, while `NodePattern` and `MatchMode` do not.
-}
precedenceSpec :: Spec
precedenceSpec = do
  let cell n = Number (mkNumberValue (T.pack (show n)) (fromIntegral n))
      -- Three levels deep, so Indent set two breadcrumbs down still shows.
      pair a b = mkArray (fromList [mkArray (fromList [a, b]), mkArray (fromList [b, a])])
      -- Complex enough to be broken across lines, so Indent shows in the output.
      topNode =
        mkObject
          ( fromList
              [ ObjectKey
                  ( String "deformGroups"
                  , mkArray
                      ( fromList
                          [ pair (cell 1) (cell 2)
                          , pair (cell 3) (cell 4)
                          ]
                      )
                  )
              ]
          )
      rulesFrom src =
        case parseDSL (textToLazyByteString src) of
          Right rs -> rs
          Left err -> error ("bad JBFL in spec: " ++ T.unpack err)
      formatWith = flip formatNode topNode . rulesFrom
      -- The second assertion is what stops the first passing for two rules that
      -- happen to format the same way.
      beats winner loser = do
        formatWith (winner <> "\n" <> loser) `shouldBe` formatWith winner
        formatWith winner `shouldNotBe` formatWith loser
      named = ".deformGroups { Indent : 1; }"
      positional = ".0 { Indent : 2; }"
      longPrefix = ".deform* { Indent : 3; }"
      shortPrefix = ".de* { Indent : 5; }"
      wildcard = ".* { Indent : 6; }"

  describe "which of several matching patterns supplies a property" $ do
    it "prefers a named key over a positional index" $ named `beats` positional
    it "prefers a positional index over a prefix key" $
      positional `beats` longPrefix
    it "prefers the longer of two prefix keys" $ longPrefix `beats` shortPrefix
    it "prefers a prefix key over a wildcard" $ shortPrefix `beats` wildcard

    -- Precedence settles one property at a time. Every shipped ruleset is
    -- written this way: `.*` carries Indent and TrailingComma for the whole
    -- file and narrower patterns add to it, so a winner that supplied its
    -- properties wholesale would strip the broad ones off every node it matched.
    -- `[4]` is the last row of the selector table in JBFL_DOCS.md, and the only
    -- one no shipped ruleset uses, so nothing else would notice it going away.
    it "matches a literal array index and prefers it over the wildcard" $
      ".deformGroups[0] { Indent : 1; }" `beats` ".deformGroups[*] { Indent : 4; }"

    it "still takes properties the winner does not set from the loser" $ do
      let winnerOnly = ".deformGroups { Indent : 1; }"
          loserOnly = ".de* { TrailingComma : Force; }"
      formatWith (winnerOnly <> "\n" <> loserOnly)
        `shouldNotBe` formatWith winnerOnly
      formatWith (winnerOnly <> "\n" <> loserOnly)
        `shouldNotBe` formatWith loserOnly

  -- A user's rules.jbfl is laid over the shipped one with `rs <> defaultRs`
  -- (`Formatting/Config.hs`), so this is how every configured install resolves
  -- its rules. The union is left-biased twice over, per pattern and per
  -- property, and a trie has to reproduce both.
  describe "combining a user ruleset with the shipped one" $ do
    let user = rulesFrom ".deformGroups { Indent : 1; }"
        shipped = rulesFrom ".deformGroups { Indent : 7; TrailingComma : Force; }"
        format = flip formatNode topNode

    it "takes the user's value and keeps the rest of the shipped one" $ do
      let merged = rulesFrom ".deformGroups { Indent : 1; TrailingComma : Force; }"
      format (user <> shipped) `shouldBe` format merged
      -- Guards the line above: without these the two rulesets could be
      -- indistinguishable and the merge would prove nothing.
      format user `shouldNotBe` format shipped
      format merged `shouldNotBe` format user

spec :: Spec
spec = do
  mapM_ formatNodeSpec specs
  matchModeSpec
  precedenceSpec

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
