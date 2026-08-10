module Core.NewlineSpec (spec) where

import JbeamEdit.Core.Newline (detectNewline)
import System.IO (Newline (..))
import Test.Hspec

spec :: Spec
spec =
  describe "detectNewline" $ do
    it "detects LF for unix line endings" $
      detectNewline "a\nb\n" `shouldBe` LF
    it "detects CRLF for windows line endings" $
      detectNewline "a\r\nb\r\n" `shouldBe` CRLF
    it "detects CRLF when line endings are mixed" $
      detectNewline "a\r\nb\n" `shouldBe` CRLF
    it "defaults to LF when there is no line ending" $
      detectNewline "abc" `shouldBe` LF
