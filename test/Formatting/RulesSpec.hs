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

  -- Which of two overlapping patterns wins is stated nowhere; it falls out of
  -- `Ord NodePattern`. The named-key case is already pinned by the glowMap
  -- rules in complex.jbfl, the length case by nothing.
  describe "overlapping patterns" $ do
    let cursor =
          NodeCursor $
            fromList
              [ObjectIndexAndKey 0 "chassis", ObjectIndexAndKey 0 "nodes"]
        ruleFor n = fromList [(SomeKey PadAmount, SomeProperty PadAmount n)]
        ruleSetOf ps = RuleSet (fromList [(NodePattern (fromList p), ruleFor n) | (p, n) <- ps])
        padAmount rs = lookupPropertyForCursor PrefixMatch PadAmount rs cursor

    it "lets the longer pattern win" $
      padAmount
        ( ruleSetOf
            [ ([AnyObjectKey], 1)
            , ([AnyObjectKey, Selector (NP.ObjectKey "nodes")], 2)
            ]
        )
        `shouldBe` Just 2

    it "lets a named key beat a wildcard of the same length" $
      padAmount
        ( ruleSetOf
            [ ([AnyObjectKey, AnyObjectKey], 1)
            , ([AnyObjectKey, Selector (NP.ObjectKey "nodes")], 2)
            ]
        )
        `shouldBe` Just 2
