module Parsing.DSLSpec (
  spec,
) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.List (isSuffixOf)
import Data.String (fromString)
import Data.Void (Void)
import Formatting.Rules
import Parsing.Common.Helpers
import Parsing.DSL
import Parsing.ParsingTestHelpers
import SpecHelper
import System.Directory (getDirectoryContents)
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Core.NodePath qualified as NP (NodeSelector (..))
import Data.ByteString qualified as BS (
  readFile,
 )

patternSelectorSpecs :: [Spec]
patternSelectorSpecs =
  map
    (applyParserSpec patternSelectorParser)
    [ (".*", AnyObjectKey)
    , ("[*]", AnyArrayIndex)
    , (".test", Selector (NP.ObjectKey "test"))
    , (".3", Selector (NP.ObjectIndex 3))
    , ("[3]", Selector (NP.ArrayIndex 3))
    ]

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

ruleSetSpec :: FilePath -> FilePath -> Spec
ruleSetSpec inFilename outFilename = do
  let inputPath = "examples/jbfl/" ++ inFilename
  input <- runIO $ BS.readFile inputPath
  output <- runIO $ readFile outFilename
  let desc = "should parse contents of " ++ inFilename ++ " to AST in " ++ outFilename
  describe desc . works $
    parseDSL input `shouldBe` Right (read output)

ruleSetSpecs :: Spec
ruleSetSpecs = do
  inputFiles <-
    runIO $ filter (isSuffixOf ".jbfl") <$> getDirectoryContents "examples/jbfl"
  let outputFile f = "examples/ast/jbfl/" ++ takeWhile (/= '.') f ++ ".hs"
  forM_ inputFiles $ \inFile -> do
    let outFile = outputFile inFile
    ruleSetSpec inFile outFile

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
    desc = "should fail parsing " <> show input

spec :: Spec
spec = do
  sequence_ $
    keyPropertyPairSpecs ++ invalidKeyPropertySpecs ++ patternSelectorSpecs
  ruleSetSpecs
