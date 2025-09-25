module Core.NodePathSpec (spec) where

import Relude.Unsafe (read)
import SpecHelper

import Core.NodePath qualified as NP

spec :: Spec
spec = describe "select" $ do
  it "selects an element by ArrayIndex, ignoring comments" $ do
    let arr =
          Array $
            fromList
              [Comment (InternalComment "c" False NextNode), String "first", String "second"]
    NP.select (NP.ArrayIndex 0) arr `shouldBe` Just (String "first")
    NP.select (NP.ArrayIndex 1) arr `shouldBe` Just (String "second")
    NP.select (NP.ArrayIndex 2) arr `shouldBe` Nothing

  it "selects a value by ObjectKey" $ do
    let obj =
          Object $ fromList [ObjectKey (String "first_key", String "first value")]
    NP.select (NP.ObjectKey "first_key") obj
      `shouldBe` Just (String "first value")
    NP.select (NP.ObjectKey "second_key") obj `shouldBe` Nothing

  it "selects a value by ObjectIndex" $ do
    let obj =
          Object $
            fromList
              [ ObjectKey (String "first_key", String "first value")
              , ObjectKey (String "second_key", String "second value")
              ]
    NP.select (NP.ObjectIndex 0) obj `shouldBe` Just (String "first value")
    NP.select (NP.ObjectIndex 1) obj `shouldBe` Just (String "second value")
    NP.select (NP.ObjectIndex 2) obj `shouldBe` Nothing

  describe "queryNodes" $ do
    let inputPath = "examples/ast/jbeam/fender.hs"
    input <- runIO $ baseReadFile inputPath
    it "queryNode can retrieve nodes" $ do
      let ast = read input
          path = fromList [NP.ObjectIndex 0, NP.ObjectKey "nodes"]
          badPath = fromList [NP.ArrayIndex 0, NP.ArrayIndex 3]
          nodes = NP.queryNodes path ast
          badNodes = NP.queryNodes badPath ast
      badNodes `shouldNotSatisfy` isJust
      nodes `shouldSatisfy` isJust
