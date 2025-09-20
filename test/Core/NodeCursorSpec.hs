module Core.NodeCursorSpec (spec) where

import Core.Node (Node (..))
import Core.NodeCursor
import Test.Hspec

import Core.NodePath qualified as NP
import Data.Sequence qualified as Seq

spec :: Spec
spec = do
  describe "newCursor" $ do
    it "creates an empty cursor" $ do
      newCursor `shouldBe` NodeCursor Seq.empty

  describe "applyCrumb" $ do
    it "appends a breadcrumb" $ do
      let cursor = newCursor
          crumb = ArrayIndex 0
          result = applyCrumb crumb cursor const (String "dummy")
      result `shouldBe` NodeCursor (Seq.singleton crumb)

  describe "applyObjCrumb" $ do
    it "transforms ArrayIndex + String into ObjectIndexAndKey" $ do
      let cursor = NodeCursor (Seq.singleton (ArrayIndex 2))
          result = applyObjCrumb (String "key") cursor const (String "dummy")
      result `shouldBe` NodeCursor (Seq.singleton (ObjectIndexAndKey (2, "key")))

  describe "compareSB" $ do
    it "matches ObjectKey correctly" $ do
      compareSB (NP.ObjectKey "foo") (ObjectIndexAndKey (0, "foo")) `shouldBe` True
      compareSB (NP.ObjectKey "bar") (ObjectIndexAndKey (0, "foo")) `shouldBe` False

    it "matches ObjectIndex correctly" $ do
      compareSB (NP.ObjectIndex 1) (ObjectIndexAndKey (1, "k")) `shouldBe` True
      compareSB (NP.ObjectIndex 2) (ObjectIndexAndKey (1, "k")) `shouldBe` False

    it "matches ArrayIndex correctly" $ do
      compareSB (NP.ArrayIndex 3) (ArrayIndex 3) `shouldBe` True
      compareSB (NP.ArrayIndex 4) (ArrayIndex 3) `shouldBe` False

    it "does not match mismatched cases" $ do
      compareSB (NP.ArrayIndex 1) (ObjectIndexAndKey (1, "x")) `shouldBe` False

  describe "comparePathAndCursor" $ do
    it "matches when path and cursor are equivalent" $ do
      let path = NP.NodePath (Seq.fromList [NP.ArrayIndex 0, NP.ObjectKey "foo"])
          cursor = NodeCursor (Seq.fromList [ArrayIndex 0, ObjectIndexAndKey (0, "foo")])
      comparePathAndCursor path cursor `shouldBe` True

    it "returns False when path and cursor differ" $ do
      let path = NP.NodePath (Seq.fromList [NP.ArrayIndex 1])
          cursor = NodeCursor (Seq.singleton (ArrayIndex 0))
      comparePathAndCursor path cursor `shouldBe` False
