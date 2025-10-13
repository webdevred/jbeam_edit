module DocumentStoreSpec (spec) where

import Language.LSP.Protocol.Types (Uri (..))
import Services.DocumentStore (delete, get, open, resetStore, update)
import Test.Hspec

spec :: Spec
spec =
  describe "DocumentStore" . before_ resetStore $
    ( do
        it "open and get returns stored text" $ do
          let u = (Uri {getUri = "file:///tmp/a.jbeam"})
              txt = "hello"
          open u txt
          m <- get u
          m `shouldBe` Just txt

        it "update overwrites existing text" $ do
          let u = (Uri {getUri = "file:///tmp/a.jbeam"})
          open u "first"
          update u "second"
          m <- get u
          m `shouldBe` Just "second"

        it "delete removes document" $ do
          let u = (Uri {getUri = "file:///tmp/b.jbeam"})
          open u "hey"
          delete u
          m <- get u
          m `shouldBe` Nothing
    )
