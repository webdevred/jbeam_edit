module Parsing.ParsingTestHelpers (
  applyParserSpec,
) where

import Data.String (fromString)
import Parsing.Common.Helpers
import SpecHelper
import Test.Hspec.Megaparsec
import Text.Megaparsec

applyParserSpec :: (Eq a, Show a) => Parser a -> (String, a) -> Spec
applyParserSpec parser = uncurry $ applySpecOnInput descFun assertParsesTo
  where
    assertParsesTo input = shouldParse . parse parser "" $ fromString input
    descFun input expResult = "should parse " <> input <> " to " <> expResult
