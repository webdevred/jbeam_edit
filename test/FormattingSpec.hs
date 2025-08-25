module FormattingSpec (
  spec,
) where

import Core.NodeCursor (newCursor)
import Data.String (fromString)
import Data.Vector (fromList)
import Formatting
import SpecHelper

numberSpec :: [(String, Node)]
numberSpec =
  [ ("123", Number 123)
  , ("123.123", Number 123.123)
  , ("-123", Number (-123))
  , ("-123.123", Number (-123.123))
  , ("0", Number 0)
  , ("0.0", Number 0.0)
  , ("-0", Number 0)
  , ("-0.0", Number 0.0)
  ]

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
arraySpec =
  [ ("[1,2,3]", Array (fromList [Number 1, Number 2, Number 3]))
  , ("[1\n 2\n 3]", Array (fromList [Number 1, Number 2, Number 3]))
  ]

objectSpec :: [(String, Node)]
objectSpec =
  [
    ( "{\"test\" : 1, \"test2\" : 2}"
    , Object
        ( fromList
            [ ObjectKey (String "test", Number 1)
            , ObjectKey (String "test2", Number 2)
            ]
        )
    )
  ,
    ( "{\"test\" : 1\n \"test2\" : 2}"
    , Object
        ( fromList
            [ ObjectKey (String "test", Number 1)
            , ObjectKey (String "test2", Number 2)
            ]
        )
    )
  ]

spec :: Spec
spec = do
  mapM_ formatNodeSpec specs
  where
    formatNodeSpec (jbeam, node) =
      applySpecOnInput
        descFun
        shouldBe
        (formatNode newRuleSet newCursor node)
        (fromString jbeam)
    descFun jbeam node = "should format " ++ node ++ " as " ++ jbeam
    specs =
      concat
        [ numberSpec
        , stringSpec
        , boolSpec
        , nullSpec
        , multilineCommentSpec
        , singlelineCommentSpec
        , arraySpec
        , objectSpec
        ]
