module Parsing.InternalSpec
  ( spec
  ) where

import Data.String (fromString)
import Data.Vector (fromList)
import Parsing.Internal
import SpecHelper
import Test.Hspec.Megaparsec
import Text.Megaparsec

numberSpec :: [(String, Node)]
numberSpec = [("123", Number 123), ("123.123", Number 123.123)]

stringSpec :: [(String, Node)]
stringSpec = [("\"test\"", String "test"), ("\"\"", String "")]

boolSpec :: [(String, Node)]
boolSpec = [("true", Bool True), ("false", Bool False)]

nullSpec :: [(String, Node)]
nullSpec = [("null", Null)]

multilineCommentSpec :: [(String, Node)]
multilineCommentSpec = [("/* test */", MultilineComment "test")]

singlelineCommentSpec :: [(String, Node)]
singlelineCommentSpec = [("// test \n", SinglelineComment "test")]

arraySpec :: [(String, Node)]
arraySpec = [("[1,2,3]", Array (fromList [Number 1, Number 2, Number 3]))
            ,("[1\n 2\n 3]", Array (fromList [Number 1, Number 2, Number 3]))]

objectSpec :: [(String, Node)]
objectSpec =
  [ ( "{\"test\" : 1, \"test2\" : 2}"
    , Object
        (fromList
           [ ObjectKey (String "test", Number 1)
           , ObjectKey (String "test2", Number 2)
           ]))
  ,( "{\"test\" : 1\n \"test2\" : 2}"
    , Object
        (fromList
           [ ObjectKey (String "test", Number 1)
           , ObjectKey (String "test2", Number 2)
           ]))
  ]

applyParserSpec :: [(String, Node)] -> [Spec]
applyParserSpec = map (uncurry $ applySpecOnInput descFun assertParsesTo)
  where
    assertParsesTo input = shouldParse . parse nodeParser "" $ fromString input 
    descFun input expResult = "should parse " <> input <> " to " <> expResult

spec :: Spec
spec =
  sequence_
    $ concatMap
        applyParserSpec
        [ numberSpec
        , stringSpec
        , boolSpec
        , nullSpec
        , multilineCommentSpec
        , singlelineCommentSpec
        , arraySpec
        , objectSpec
        ]
