module Parsing.JbeamSpec (
  spec,
) where

import Parsing.Common.Helpers
import Parsing.Jbeam
import Relude.Unsafe (read)
import SpecHelper
import Test.Hspec.Megaparsec

import Text.Megaparsec qualified as MP

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
multilineCommentSpec =
  [
    ( "/* test */"
    , Comment
        (InternalComment {cText = "test", cMultiline = True, assocWithPrior = False})
    )
  ]

singlelineCommentSpec :: [(String, Node)]
singlelineCommentSpec =
  [
    ( "// test \n"
    , Comment
        (InternalComment {cText = "test", cMultiline = False, assocWithPrior = False})
    )
  ,
    ( "[\"test\", \"test\" // cool comment \n ]"
    , Array
        ( fromList
            [ String "test"
            , String "test"
            , Comment
                ( InternalComment
                    { cText = "cool comment"
                    , cMultiline = False
                    , assocWithPrior = True
                    }
                )
            ]
        )
    )
  ]

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

invalidSpec :: Spec
invalidSpec =
  describe "should fail when input is malformed" . works $
    parseNodesState nodeParser "[1,2,a,4]"
      `shouldFailWith` err 6 (utok (toWord8 'a') <> expLabels)
  where
    expLabels = foldMap elabel ["a valid scalar", "object", "array"]

invalidNumberSpec :: Spec
invalidNumberSpec =
  describe
    "should fail parsing Number when there is space after the negative sign"
    . works
    $ parseNodesState numberParser "- 0.3"
      `shouldFailWith` err 1 (utok (toWord8 ' ') <> elabel "digit")

topNodeSpec :: FilePath -> FilePath -> Spec
topNodeSpec inFilename outFilename = do
  let inputPath = "examples/jbeam/" ++ inFilename
  input <- runIO $ readFileBS inputPath
  output <- runIO $ baseReadFile outFilename
  let desc = "should parse contents of " ++ inFilename ++ " to AST in " ++ outFilename
  describe desc . works $ do
    parseNodes input `shouldBe` Right (read output)
    parseNodesState nodeParser input `shouldParse` read output

topNodeSpecs :: Spec
topNodeSpecs = do
  inputFiles <-
    runIO $ listFilesInDir "examples/jbeam"
  let outputFile inFile = "examples/ast/jbeam/" ++ takeWhile (/= '.') inFile ++ ".hs"
      testInputFile inFile = topNodeSpec inFile (outputFile inFile)
  mapM_ testInputFile inputFiles

parseNodesState'
  :: JbeamParser a
  -> String
  -> Either (MP.ParseErrorBundle ByteString Void) a
parseNodesState' parser = parseNodesState parser . fromString

applyParserSpec :: (Eq a, Show a) => JbeamParser a -> (String, a) -> Spec
applyParserSpec parser = uncurry $ applySpecOnInput descFun assertParsesTo
  where
    assertParsesTo input = shouldParse (parseNodesState' parser input)
    descFun input expResult = "should parse " <> input <> " to " <> expResult

invalidTopNodeSpec :: Spec
invalidTopNodeSpec = do
  let inputPath = "examples/invalid_jbeam/invalid_fender.jbeam"
  input <- runIO $ readFileBS inputPath
  let desc = "should fail for content in " ++ inputPath
  describe desc . works $
    parseNodes input
      `shouldBe` Left
        "got: '{', expecting '}', comma, comment, string or white space somewhere close to {\"collision\" : true}, on line 9"

spec :: Spec
spec = do
  mapM_ (applyParserSpec nodeParser) specs
  invalidSpec
  invalidNumberSpec
  invalidTopNodeSpec
  topNodeSpecs
  where
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
