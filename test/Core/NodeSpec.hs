module Core.NodeSpec (spec) where

import Data.Vector qualified as V
import SpecHelper

spec :: Spec
spec = do
  describe "isCommentNode" . works $
    ( do
        isCommentNode (Comment (InternalComment "test" False NextNode False))
          `shouldBe` True
        isCommentNode (Number (IntValue 123)) `shouldBe` False
    )
  describe "isObjectNode" . works $
    ( do
        isObjectNode
          (Object (V.singleton (ObjectKey (String "test", Number (IntValue 1)))))
          `shouldBe` True
        isObjectNode (Number (IntValue 123)) `shouldBe` False
    )
  describe "isStringNode" . works $
    ( do
        isStringNode (String "test") `shouldBe` True
        isStringNode (Number (IntValue 123)) `shouldBe` False
    )
  describe "isNumberNode" . works $
    ( do
        isNumberNode (Number (IntValue 123)) `shouldBe` True
        isNumberNode (String "123") `shouldBe` False
    )
  describe "isComplexNode" . works $
    ( do
        isComplexNode (Number (IntValue 123)) `shouldBe` False
        isComplexNode
          ( Object
              (V.singleton (ObjectKey (String "test", Number (IntValue 1))))
          )
          `shouldBe` False
        isComplexNode
          ( Object
              ( V.fromList
                  [ ObjectKey (String "test", Number (IntValue 1))
                  , ObjectKey (String "test", Number (IntValue 1))
                  ]
              )
          )
          `shouldBe` True
        isComplexNode (Array (V.fromList [String "test", Number (IntValue 1)]))
          `shouldBe` True
    )
