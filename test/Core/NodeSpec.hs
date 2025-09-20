module Core.NodeSpec (spec) where

import SpecHelper

import Data.Vector qualified as V

spec :: Spec
spec = do
  describe "isCommentNode" $ do
    works $ do
      isCommentNode (Comment (InternalComment "test" False)) `shouldBe` True
      isCommentNode (Number 123) `shouldBe` False
  describe "isObjectNode" $ do
    works $ do
      isObjectNode (Object (V.singleton (ObjectKey (String "test", Number 1))))
        `shouldBe` True
      isObjectNode (Number 123) `shouldBe` False
  describe "isStringNode" $ do
    works $ do
      isStringNode (String "test") `shouldBe` True
      isStringNode (Number 123) `shouldBe` False
  describe "isNumberNode" $ do
    works $ do
      isNumberNode (Number 123) `shouldBe` True
      isNumberNode (String "123") `shouldBe` False
  describe "isComplexNode" $ do
    works $ do
      isComplexNode (Number 123) `shouldBe` False
      isComplexNode (Object (V.singleton (ObjectKey (String "test", Number 1))))
        `shouldBe` True
      isComplexNode (Array (V.fromList [String "test", Number 1])) `shouldBe` True
