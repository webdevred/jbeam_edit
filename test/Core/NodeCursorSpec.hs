module Core.NodeCursorSpec (spec) where

import Data.Sequence qualified as Seq
import JbeamEdit.Core.NodeCursor
import JbeamEdit.Core.NodePath qualified as NP
import Test.Hspec

spec :: Spec
spec = do
  describe "newCursor" . it "creates an empty cursor" $
    newCursor `shouldBe` NodeCursor Seq.empty

  describe "compareSB" $ do
    it "matches ObjectKey correctly" $ do
      compareSB (NP.ObjectKey "foo") (ObjectIndexAndKey 0 "foo") `shouldBe` True
      compareSB (NP.ObjectKey "bar") (ObjectIndexAndKey 0 "foo") `shouldBe` False

    it "matches ObjectIndex correctly" $ do
      compareSB (NP.ObjectIndex 1) (ObjectIndexAndKey 1 "k") `shouldBe` True
      compareSB (NP.ObjectIndex 2) (ObjectIndexAndKey 1 "k") `shouldBe` False

    it "matches ArrayIndex correctly" $ do
      compareSB (NP.ArrayIndex 3) (ArrayIndex 3) `shouldBe` True
      compareSB (NP.ArrayIndex 4) (ArrayIndex 3) `shouldBe` False

    it "does not match mismatched cases" $
      compareSB (NP.ArrayIndex 1) (ObjectIndexAndKey 1 "x") `shouldBe` False

  describe "comparePathAndCursor" $ do
    it "matches when path and cursor are equivalent" $ do
      let path = NP.NodePath (Seq.fromList [NP.ArrayIndex 0, NP.ObjectKey "foo"])
          cursor = NodeCursor (Seq.fromList [ArrayIndex 0, ObjectIndexAndKey 0 "foo"])
      comparePathAndCursor path cursor `shouldBe` True

    it "returns False when path and cursor differ" $ do
      let path = NP.NodePath (Seq.fromList [NP.ArrayIndex 1])
          cursor = NodeCursor (Seq.singleton (ArrayIndex 0))
      comparePathAndCursor path cursor `shouldBe` False
