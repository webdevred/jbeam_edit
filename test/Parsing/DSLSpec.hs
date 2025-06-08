module Parsing.DSLSpec
  ( spec
  ) where

import Data.String (fromString)
import Formatting.Rules
import Parsing.Common.Helpers
import Parsing.DSL
import SpecHelper
import Test.Hspec.Megaparsec
import Text.Megaparsec

intProperties :: [(String, (SomeKey, SomeProperty))]
intProperties =
  [ (" PadDecimals : 3;", (SomeKey PadDecimals, SomeProperty PadDecimals 3))
  , ("PadAmount : 8;", (SomeKey PadAmount, SomeProperty PadAmount 8))
  ]

boolProperties :: [(String, (SomeKey, SomeProperty))]
boolProperties =
  [ ( "NoComplexNewLine : true;"
    , (SomeKey NoComplexNewLine, SomeProperty NoComplexNewLine True))
  ]

keyPropertyPairSpecs :: [Spec]
keyPropertyPairSpecs = map applyPropertySpec (boolProperties ++ intProperties)
  where
    applyPropertySpec s@(_, (p, _)) = applyParserSpec keyPropertyPairParser s

applyParserSpec :: (Eq a, Show a) => Parser a -> (String, a) -> Spec
applyParserSpec parser = uncurry $ applySpecOnInput descFun assertParsesTo
  where
    assertParsesTo input = shouldParse . parse parser "" $ fromString input
    descFun input expResult = "should parse " <> input <> " to " <> expResult

spec :: Spec
spec = sequence_ keyPropertyPairSpecs
