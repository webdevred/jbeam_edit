module Formatting.RulesSpec (spec) where

import GHC.IsList (fromList)
import JbeamEdit.Core.NodeCursor (NodeBreadcrumb (..), NodeCursor (..))
import JbeamEdit.Core.NodePath qualified as NP
import JbeamEdit.Formatting
import JbeamEdit.Formatting.Rules
import SpecHelper

spec :: Spec
spec = do
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

  -- A pattern matches only once it has been consumed whole, and leftover
  -- breadcrumbs are allowed under PrefixMatch alone. A lookup that answers at
  -- the wrong depth silently changes formatting everywhere, since every
  -- property the formatter reads comes through one of the two modes.
  describe "matching a pattern against a cursor" $ do
    let cursorAt crumbs = NodeCursor (fromList crumbs)
        nodesCursor =
          cursorAt [ObjectIndexAndKey 0 "part", ObjectIndexAndKey 0 "nodes"]
        ruleSetFor p =
          RuleSet
            ( fromList
                [
                  ( NodePattern (fromList p)
                  , fromList [(SomeKey PadAmount, SomeProperty PadAmount 7)]
                  )
                ]
            )
        found mode p = lookupPropertyForCursor mode PadAmount (ruleSetFor p)

    it "matches a pattern of the same length in both modes" $ do
      let p = [AnyObjectKey, Selector (NP.ObjectKey "nodes")]
      found PrefixMatch p nodesCursor `shouldBe` Just 7
      found ExactMatch p nodesCursor `shouldBe` Just 7

    it "matches a shorter pattern only as a prefix" $ do
      let p = [AnyObjectKey]
      found PrefixMatch p nodesCursor `shouldBe` Just 7
      found ExactMatch p nodesCursor `shouldBe` Nothing

    it "never matches a pattern longer than the cursor" $ do
      let p =
            [ AnyObjectKey
            , Selector (NP.ObjectKey "nodes")
            , AnyArrayIndex
            ]
      found PrefixMatch p nodesCursor `shouldBe` Nothing
      found ExactMatch p nodesCursor `shouldBe` Nothing

    it "keeps the two wildcards apart" $ do
      let atArray = cursorAt [ObjectIndexAndKey 0 "part", ArrayIndex 0]
          atKey = cursorAt [ObjectIndexAndKey 0 "part", ObjectIndexAndKey 0 "k"]
          anyKeyThen w = [AnyObjectKey, w]
      found ExactMatch (anyKeyThen AnyArrayIndex) atArray `shouldBe` Just 7
      found ExactMatch (anyKeyThen AnyObjectKey) atArray `shouldBe` Nothing
      found ExactMatch (anyKeyThen AnyObjectKey) atKey `shouldBe` Just 7
      found ExactMatch (anyKeyThen AnyArrayIndex) atKey `shouldBe` Nothing
