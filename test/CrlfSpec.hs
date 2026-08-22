module CrlfSpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Data.Either (isRight)
import JbeamEdit.Parsing.Jbeam (parseNodes)
import Test.Hspec

carriageReturn :: LBS.ByteString -> Bool
carriageReturn = LBS.elem 13

{- | Line endings are the file's business, not the content's, so the same file
read as CRLF and as LF has to parse to the same tree. Every other fixture here
is LF while 4793 of the 4943 jbeam files in the stock vehicles are CRLF, so
without this one the ordinary suite never sees a carriage return at all, and
the parser has had a CRLF-specific bug in a block comment before.
-}
spec :: Spec
spec = do
  crlf <- runIO $ LBS.readFile "examples/regression_jbeam/crlf-line-endings.jbeam"
  let lf = LBS.filter (/= 13) crlf
  describe "a jbeam file with CRLF line endings" $ do
    it "still has its carriage returns" $
      -- Guards against a checkout or a .gitattributes change quietly
      -- normalising the fixture, which would leave the rest passing vacuously.
      crlf `shouldSatisfy` carriageReturn

    it "parses to the same tree as the same file with LF" $ do
      lf `shouldNotSatisfy` carriageReturn
      parseNodes crlf `shouldBe` parseNodes lf

    it "parses at all" $
      parseNodes crlf `shouldSatisfy` isRight
