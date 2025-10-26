module Core.NodeSpec (spec) where

import GHC.IsList
import SpecHelper

spec :: Spec
spec = do
  describe "isCommentNode" . works $
    ( do
        isCommentNode (Comment (InternalComment "test" False NextNode)) `shouldBe` True
        isCommentNode (Number 123) `shouldBe` False
    )
  describe "isObjectNode" . works $
    ( do
        isObjectNode (Object (fromList [ObjectKey (String "test", Number 1)]))
          `shouldBe` True
        isObjectNode (Number 123) `shouldBe` False
    )
  describe "isStringNode" . works $
    ( do
        isStringNode (String "test") `shouldBe` True
        isStringNode (Number 123) `shouldBe` False
    )
  describe "isNumberNode" . works $
    ( do
        isNumberNode (Number 123) `shouldBe` True
        isNumberNode (String "123") `shouldBe` False
    )
  describe "isComplexNode" . works $
    ( do
        isComplexNode (Number 123) `shouldBe` False
        isComplexNode (Object (fromList [ObjectKey (String "test", Number 1)]))
          `shouldBe` True
        isComplexNode (Array (fromList [String "test", Number 1])) `shouldBe` True
    )
