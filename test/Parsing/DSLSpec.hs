module Parsing.DSLSpec (
  spec,
) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS (fromStrict, readFile)
import Data.Functor.Identity (Identity (..))
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Void (Void)
import JbeamEdit.Core.NodePath qualified as NP (NodeSelector (..))
import JbeamEdit.Formatting.Rules
import JbeamEdit.Parsing.Common.Helpers
import JbeamEdit.Parsing.DSL
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
    , (".test*", Selector (NP.ObjectPrefixKey "test"))
    , (".deformGroups_oilPan*", Selector (NP.ObjectPrefixKey "deformGroups_oilPan"))
    , (".3", Selector (NP.ObjectIndex 3))
    , ("[3]", Selector (NP.ArrayIndex 3))
    ]

intProperties :: [(String, (SomeKey, SomeProperty))]
intProperties =
  [ ("PadDecimals : 3;", (SomeKey PadDecimals, SomeProperty PadDecimals 3))
  , ("PadAmount : 8;", (SomeKey PadAmount, SomeProperty PadAmount 8))
  , ("Indent : 4;", (SomeKey Indent, SomeProperty Indent 4))
  ]

enumProperties :: [(String, (SomeKey, SomeProperty))]
enumProperties =
  [
    ( "ComplexNewLine : Force;"
    , (SomeKey ComplexNewLine, SomeProperty ComplexNewLine Force)
    )
  ,
    ( "ComplexNewLine : None;"
    , (SomeKey ComplexNewLine, SomeProperty ComplexNewLine None)
    )
  ]

deprecatedProperties :: [(String, (SomeKey, SomeProperty))]
deprecatedProperties =
  [
    ( "NoComplexNewLine : true;"
    , (SomeKey ComplexNewLine, SomeProperty ComplexNewLine None)
    )
  ,
    ( "ForceComplexNewLine : true;"
    , (SomeKey ComplexNewLine, SomeProperty ComplexNewLine Force)
    )
  ]

ruleSetSpec :: FilePath -> FilePath -> Spec
ruleSetSpec inFilename outFilename = do
  let inputPath = "examples/jbfl/" ++ inFilename
  input <- runIO $ BS.readFile inputPath
  output <- runIO $ readFile outFilename
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
keyPropertyPairSpecs =
  map applyPropertySpec (enumProperties ++ deprecatedProperties ++ intProperties)
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

invalidEnumProperties :: [(String, ParseError ByteString Void)]
invalidEnumProperties =
  [
    ( "ComplexNewLine : 3;"
    , err 17 (utoks "3;" <> expLabels ["Force or None", "white space"])
    )
  ]

invalidKeyPropertySpecs :: [Spec]
invalidKeyPropertySpecs =
  map
    (assertParserFailure keyPropertyPairParser)
    (invalidEnumProperties ++ invalidIntProperties)

applyParserSpec
  :: (Eq a, Show a) => ParsecT Void ByteString Identity a -> (String, a) -> Spec
applyParserSpec parser = uncurry $ applySpecOnInput descFun assertParsesTo
  where
    assertParsesTo input = shouldParse . parse parser "" . BS.fromStrict . encodeUtf8 $ T.pack input
    descFun input expResult = "should parse " <> input <> " to " <> expResult

assertParserFailure
  :: Show a => JbflParser a -> (String, ParseError ByteString Void) -> Spec
assertParserFailure parser (input, expError) =
  describe desc . works $
    parse parser "" (textToLazyByteString input)
      `shouldFailWith` expError
  where
    desc = "should fail parsing " <> show input

spec :: Spec
spec = do
  sequence_ $
    keyPropertyPairSpecs ++ invalidKeyPropertySpecs ++ patternSelectorSpecs
  ruleSetSpecs
