{-# LANGUAGE OverloadedStrings #-}

module Core.NodePathSpec (spec) where

import Core.NodePath
import Test.Hspec

import Core.Node qualified as N

spec :: Spec
spec = do
  describe "select" $ do
    it "selects an element by ArrayIndex, ignoring comments" $ do
      let arr =
            N.Array $
              fromList
                [N.Comment (N.InternalComment "c" False), N.String "first", N.String "second"]
      select (ArrayIndex 0) arr `shouldBe` Just (N.String "first")
      select (ArrayIndex 1) arr `shouldBe` Just (N.String "second")
      select (ArrayIndex 2) arr `shouldBe` Nothing

    it "selects a value by ObjectKey" $ do
      let obj =
            N.Object $ fromList [N.ObjectKey (N.String "first_key", N.String "first value")]
      select (ObjectKey "first_key") obj `shouldBe` Just (N.String "first value")
      select (ObjectKey "second_key") obj `shouldBe` Nothing

    it "selects a value by ObjectIndex" $ do
      let obj =
            N.Object $
              fromList
                [ N.ObjectKey (N.String "first_key", N.String "first value")
                , N.ObjectKey (N.String "second_key", N.String "second value")
                ]
      select (ObjectIndex 0) obj `shouldBe` Just (N.String "first value")
      select (ObjectIndex 1) obj `shouldBe` Just (N.String "second value")
      select (ObjectIndex 2) obj `shouldBe` Nothing
