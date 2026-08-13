module Core.NodeSpec (spec) where

import Data.Vector qualified as V
import SpecHelper

spec :: Spec
spec = do
  describe "isCommentNode" . works $
    ( do
        isCommentNode (Comment (InternalComment "test" False NextNode False))
          `shouldBe` True
        isCommentNode (Number (mkNumberValueNormalized 123)) `shouldBe` False
    )
  describe "isObjectNode" . works $
    ( do
        isObjectNode
          ( mkObject
              (V.singleton (ObjectKey (String "test", Number (mkNumberValueNormalized 1))))
          )
          `shouldBe` True
        isObjectNode (Number (mkNumberValueNormalized 123)) `shouldBe` False
    )
  describe "isStringNode" . works $
    ( do
        isStringNode (String "test") `shouldBe` True
        isStringNode (Number (mkNumberValueNormalized 123)) `shouldBe` False
    )
  describe "isNumberNode" . works $
    ( do
        isNumberNode (Number (mkNumberValueNormalized 123)) `shouldBe` True
        isNumberNode (String "123") `shouldBe` False
    )
  describe "isComplexNode" . works $
    ( do
        isComplexNode (Number (mkNumberValueNormalized 123)) `shouldBe` False
        isComplexNode
          ( mkObject
              (V.singleton (ObjectKey (String "test", Number (mkNumberValueNormalized 1))))
          )
          `shouldBe` False
        isComplexNode
          ( mkObject
              ( V.fromList
                  [ ObjectKey (String "test", Number (mkNumberValueNormalized 1))
                  , ObjectKey (String "test", Number (mkNumberValueNormalized 1))
                  ]
              )
          )
          `shouldBe` True
        isComplexNode
          (mkArray (V.fromList [String "test", Number (mkNumberValueNormalized 1)]))
          `shouldBe` True
    )
