module Formatting.RulesSpec (spec) where

import Data.Text qualified as T
import GHC.IsList (fromList)
import JbeamEdit.Core.NodeCursor (NodeBreadcrumb (..), NodeCursor (..))
import JbeamEdit.Formatting
import JbeamEdit.Formatting.Rules
import SpecHelper

spec :: Spec
spec = do
  precedenceSpec

  describe "SomeKey & SomeProperty" $ do
    it "Eq works for same PropertyKey" $
      SomeKey PadAmount == SomeKey PadAmount `shouldBe` True

    it "Eq works for different PropertyKey" $
      SomeKey PadAmount == SomeKey Indent `shouldBe` False

    it "Show/Read roundtrip for SomeProperty" $ do
      let prop = SomeProperty PadAmount 5
      read (show prop) `shouldBe` prop

  describe "lookupKey" $ do
    it "finds an existing key" $
      lookupKey "PadAmount" allProperties `shouldBe` Just (SomeKey PadAmount)

    it "returns Nothing for missing key" $
      lookupKey "NotAKey" allProperties `shouldBe` Nothing

  describe "applyPadLogic" $ do
    let fakeNode = Number (mkNumberValue "123.5" 123.5)
        ruleSet =
          fromList
            [ (SomeKey PadAmount, SomeProperty PadAmount 7)
            , (SomeKey PadDecimals, SomeProperty PadDecimals 2)
            ]
    it "applies PadAmount and PadDecimals" $
      applyPadLogic (formatScalarNode False) ruleSet fakeNode `shouldBe` "123.50 "

  -- Whether a property reaches below the node its rule names is decided by the
  -- property, not by the caller: `prefixProperties` goes to rsBelow, the rest
  -- to rsHere.
  describe "matching a pattern against a cursor" $ do
    let cursorAt crumbs = NodeCursor (fromList crumbs)
        nodesCursor =
          cursorAt [ObjectIndexAndKey 0 "part", ObjectIndexAndKey 0 "nodes"]
        rulesFor pat = rulesFromSource (pat ++ " { PadAmount: 7; AutoPad: true; }")
        cascading pat = lookupPropertyForCursor PadAmount (rulesFor pat)
        hereOnly pat = lookupPropertyForCursor AutoPad (rulesFor pat)

    it "answers both kinds at the node the pattern names" $ do
      cascading ".*.nodes" nodesCursor `shouldBe` Just 7
      hereOnly ".*.nodes" nodesCursor `shouldBe` Just True

    it "carries a cascading property below that node, and nothing else" $ do
      cascading ".*" nodesCursor `shouldBe` Just 7
      hereOnly ".*" nodesCursor `shouldBe` Nothing

    it "never answers for a pattern longer than the cursor" $ do
      cascading ".*.nodes[*]" nodesCursor `shouldBe` Nothing
      hereOnly ".*.nodes[*]" nodesCursor `shouldBe` Nothing

    -- The one selector a trie cannot key on directly: the stored text is a
    -- prefix of the breadcrumb, not equal to it.
    it "matches a prefix key against the rest of the breadcrumb" $ do
      let p k = ".*." ++ k ++ "*"
          atDeformGroups =
            cursorAt
              [ObjectIndexAndKey 0 "part", ObjectIndexAndKey 0 "deformGroups"]
      hereOnly (p "deform") atDeformGroups `shouldBe` Just True
      hereOnly (p "deformGroups") atDeformGroups `shouldBe` Just True
      hereOnly (p "deformGroupsAndMore") atDeformGroups `shouldBe` Nothing
      hereOnly (p "eform") atDeformGroups `shouldBe` Nothing

    it "keeps the two wildcards apart" $ do
      let atArray = cursorAt [ObjectIndexAndKey 0 "part", ArrayIndex 0]
          atKey = cursorAt [ObjectIndexAndKey 0 "part", ObjectIndexAndKey 0 "k"]
      hereOnly ".*[*]" atArray `shouldBe` Just True
      hereOnly ".*.*" atArray `shouldBe` Nothing
      hereOnly ".*.*" atKey `shouldBe` Just True
      hereOnly ".*[*]" atKey `shouldBe` Nothing

{- | When several patterns match the same node, the more specific one supplies
the property: a named key, then a positional index, then a prefix key with the
longer prefix, then the wildcards.
-}
precedenceSpec :: Spec
precedenceSpec = do
  let cell :: Int -> Node
      cell n = Number (mkNumberValue (T.pack (show n)) (fromIntegral n))
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
      rulesFrom = rulesFromSource
      formatWith = flip formatNode topNode . rulesFrom
      -- Both orders, because writing the winner first would otherwise let a
      -- lookup that just takes the first parsed rule pass. The last assertion
      -- stops the others passing for two rules that format the same way.
      beats winner loser = do
        formatWith (winner <> "\n" <> loser) `shouldBe` formatWith winner
        formatWith (loser <> "\n" <> winner) `shouldBe` formatWith winner
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

    -- Resolution is per property, as in CSS: two matching rules that set
    -- different properties both apply, and only a property they both set is
    -- decided by specificity. Read through the lookup rather than the output,
    -- since one formatted file cannot show which rule each property came from.
    it "takes each property from its own most specific match" $ do
      let cur = NodeCursor (fromList [ObjectIndexAndKey 0 "deformGroups"])
          both a b = rulesFromSource (a <> "\n" <> b)
          short = ".de* { Indent: 2; PadAmount: 4; }"
          long = ".deform* { PadAmount: 8; }"
      lookupPropertyForCursor Indent (both short long) cur `shouldBe` Just 2
      lookupPropertyForCursor Indent (both long short) cur `shouldBe` Just 2
      lookupPropertyForCursor PadAmount (both short long) cur `shouldBe` Just 8
      lookupPropertyForCursor PadAmount (both long short) cur `shouldBe` Just 8
    it "prefers a prefix key over a wildcard" $ shortPrefix `beats` wildcard

    -- Precedence settles one property at a time. Every shipped ruleset is
    -- written this way: `.*` carries Indent and TrailingComma for the whole
    -- file and narrower patterns add to it, so a winner that supplied its
    -- properties wholesale would strip the broad ones off every node it matched.
    -- `[4]` is the last row of the selector table in JBFL_DOCS.md, and the only
    -- one no shipped ruleset uses, so nothing else would notice it going away.
    -- A prefix key has to reach below the node it names, like any other
    -- selector under a prefix match. It is also the one a trie cannot key on
    -- directly, so it is the likeliest of them to lose that reach.
    it "cascades from a prefix key the same way a named key does" $ do
      let byPrefix = formatWith ".deform* { Indent : 1; }"
      byPrefix `shouldBe` formatWith ".deformGroups { Indent : 1; }"
      byPrefix `shouldNotBe` formatWith ".deformGroups { Indent : 6; }"

    -- Length is settled before specificity, and it is the one rung with no
    -- shorthand: the two patterns reach different sets of nodes, so the winner
    -- alone cannot be the expected value. The outer array is indented by the
    -- shorter rule, the inner ones by the longer.
    it "settles length before specificity" $ do
      let shorter = ".deformGroups { Indent : 7; }"
          longer = ".deformGroups[*] { Indent : 1; }"
          both = shorter <> "\n" <> longer
      formatWith both
        `shouldBe` "{\n    \"deformGroups\" : [\n           [\n            [1, 2],\n            [2, 1]\n           ],\n           [\n            [3, 4],\n            [4, 3]\n           ]\n    ]\n}\n"
      formatWith both `shouldNotBe` formatWith shorter
      formatWith both `shouldNotBe` formatWith longer

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
  -- The other direction from the merge below: within one file the later rule
  -- wins, while a user ruleset beats the shipped one it is merged with.
  describe "two rules with the same pattern"
    . it "takes the value from the later one" $ do
    let twice = rulesFrom ".deformGroups { Indent : 1; }\n.deformGroups { Indent : 2; }"
        cur = NodeCursor (fromList [ObjectIndexAndKey 0 "deformGroups"])
    lookupPropertyForCursor Indent twice cur `shouldBe` Just 2

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
