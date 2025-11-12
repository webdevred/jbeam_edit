module Formatting.RulesSpec (spec) where

import GHC.IsList (fromList)
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
    let fakeNode = Number 123
        ruleSet =
          fromList
            [ (SomeKey PadAmount, SomeProperty PadAmount 7)
            , (SomeKey PadDecimals, SomeProperty PadDecimals 2)
            ]
    it "applies PadAmount and PadDecimals" $
      applyPadLogic formatScalarNode ruleSet fakeNode `shouldBe` " 123.00"
