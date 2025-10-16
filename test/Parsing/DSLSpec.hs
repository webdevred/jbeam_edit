module Parsing.DSLSpec (
  spec,
) where

import Core.NodePath qualified as NP (NodeSelector (..))
import Formatting.Rules
import Parsing.Common.Helpers
import Parsing.DSL
import Relude.Unsafe (read)
import SpecHelper
import Test.Hspec.Megaparsec
import Text.Megaparsec

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
  , ("Indent : 4;", (SomeKey Indent, SomeProperty Indent 4))
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
  input <- runIO $ readFileBS inputPath
  output <- runIO $ baseReadFile outFilename
  let desc = "should parse contents of " ++ inFilename ++ " to AST in " ++ outFilename
  describe desc . works $ do
    parseDSL input `shouldBe` Right (read output)
    parse ruleSetParser "" input `shouldParse` read output

ruleSetSpecs :: Spec
ruleSetSpecs = do
  inputFiles <-
    runIO $ listFilesInDir "examples/jbfl"
  let outputFile inFile = "examples/ast/jbfl/" ++ takeWhile (/= '.') inFile ++ ".hs"
      testInputFile inFile = ruleSetSpec inFile (outputFile inFile)
  mapM_ testInputFile inputFiles

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
  ,
    ( "Indent : true;"
    , err 9 (utok (toWord8 't') <> expLabels ["integer", "white space"])
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

applyParserSpec
  :: (Eq a, Show a) => ParsecT Void ByteString Identity a -> (String, a) -> Spec
applyParserSpec parser = uncurry $ applySpecOnInput descFun assertParsesTo
  where
    assertParsesTo input = shouldParse . parse parser "" $ fromString input
    descFun input expResult = "should parse " <> input <> " to " <> expResult

assertParserFailure
  :: Show a => JbflParser a -> (String, ParseError ByteString Void) -> Spec
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
