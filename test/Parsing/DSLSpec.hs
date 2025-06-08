module Parsing.DSLSpec (
  spec,
) where

import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.Void (Void)
import Formatting.Rules
import Parsing.Common.Helpers
import Parsing.DSL
import Parsing.ParsingTestHelpers
import SpecHelper
import Test.Hspec.Megaparsec
import Text.Megaparsec

intProperties :: [(String, (SomeKey, SomeProperty))]
intProperties =
  [ ("PadDecimals : 3;", (SomeKey PadDecimals, SomeProperty PadDecimals 3))
  , ("PadAmount : 8;", (SomeKey PadAmount, SomeProperty PadAmount 8))
  ]

boolProperties :: [(String, (SomeKey, SomeProperty))]
boolProperties =
  [
    ( "NoComplexNewLine : true;"
    , (SomeKey NoComplexNewLine, SomeProperty NoComplexNewLine True)
    )
  ]

expLabels :: [String] -> ET ByteString
expLabels = foldMap elabel

keyPropertyPairSpecs :: [Spec]
keyPropertyPairSpecs = map applyPropertySpec (boolProperties ++ intProperties)
  where
    applyPropertySpec = applyParserSpec keyPropertyPairParser

invalidIntProperties :: [(String, ParseError ByteString Void)]
invalidIntProperties =
  [
    ( "PadDecimals : true;"
    , err 14 (utok (toWord8 't') <> expLabels ["integer", "white space"])
    )
  ,
    ( "PadAmount : true;"
    , err 12 (utok (toWord8 't') <> expLabels ["integer", "white space"])
    )
  ]

invalidBoolProperties :: [(String, ParseError ByteString Void)]
invalidBoolProperties =
  [
    ( "NoComplexNewLine : 3;"
    , err 19 (utoks "3;" <> expLabels ["bool", "white space"])
    )
  ]

invalidKeyPropertySpecs :: [Spec]
invalidKeyPropertySpecs =
  map
    (assertParserFailure keyPropertyPairParser)
    (invalidBoolProperties ++ invalidIntProperties)

assertParserFailure
  :: Show a => Parser a -> (String, ParseError ByteString Void) -> Spec
assertParserFailure parser (input, expError) =
  describe desc . works $
    parse parser "" (fromString input) `shouldFailWith` expError
  where
    desc = "should parse on invalid " <> input

spec :: Spec
spec = sequence_ $ keyPropertyPairSpecs ++ invalidKeyPropertySpecs
