module Parsing.InternalSpec
  ( spec
  ) where

import Parsing.Internal
import SpecHelper
import Test.Hspec.Megaparsec
import Text.Megaparsec

numberSpec :: Spec
numberSpec = do
  describe "should parse integers as Numbers" . it "works"
    $ parse numberParser "" "123" `shouldParse` Number 123
  describe "should parse floats as Numbers" . it "works"
    $ parse numberParser "" "123.123" `shouldParse` Number 123.123

stringSpec :: Spec
stringSpec = do
  describe "should parse Strings" . it "works"
    $ parse stringParser "" "\"test\"" `shouldParse` String "test"
  describe "should parse empty Strings" . it "works"
    $ parse stringParser "" "\"\"" `shouldParse` String ""

boolSpec :: Spec
boolSpec = do
  describe "should parse true as Bool True" . it "works"
    $ parse boolParser "" "true" `shouldParse` Bool True
  describe "should parse false as Bool False" . it "works"
    $ parse boolParser "" "false" `shouldParse` Bool False

multilineCommentSpec :: Spec
multilineCommentSpec = do
  describe "should parse /* test */ as MultilineComment test" . works
    $ parse multilineCommentParser "" "/* test */"
        `shouldParse` MultilineComment "test"

singlelineCommentSpec :: Spec
singlelineCommentSpec = do
  describe "should parse // test \\n as SinglelineComment test" . works
    $ parse singlelineCommentParser "" "// test \n"
        `shouldParse` SinglelineComment "test"

spec :: Spec
spec =
  sequence_
    [ numberSpec
    , stringSpec
    , boolSpec
    , multilineCommentSpec
    , singlelineCommentSpec
    ]
