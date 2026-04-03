module Parsing.JbeamSpec (
  spec,
) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BS (readFile)
import Data.Vector (fromList)
import Data.Void (Void)
import JbeamEdit.Parsing.Common.Helpers
import JbeamEdit.Parsing.Jbeam
import SpecHelper
import Test.Hspec.Megaparsec
import Text.Megaparsec qualified as MP

numberSpec :: [(String, Node)]
numberSpec =
  [ ("123", Number (mkNumberValue "123" 123))
  , ("123.123", Number (mkNumberValue "123.123" 123.123))
  , ("-123", Number (mkNumberValue "-123" (-123)))
  , ("-123.123", Number (mkNumberValue "-123.123" (-123.123)))
  , ("0", Number (mkNumberValue "0" 0))
  , ("0.0", Number (mkNumberValue "0.0" 0.0))
  , ("-0", Number (mkNumberValue "-0" 0))
  , ("-0.0", Number (mkNumberValue "-0.0" 0.0))
  , ("+9", Number (mkNumberValue "+9" 9))
  , ("+0", Number (mkNumberValue "+0" 0))
  , ("+123.5", Number (mkNumberValue "+123.5" 123.5))
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
        ( InternalComment
            { cText = "test"
            , cMultiline = True
            , cAssociationDirection = NextNode
            , cHadNewlineBefore = False
            }
        )
    )
  ]

singlelineCommentSpec :: [(String, Node)]
singlelineCommentSpec =
  [
    ( "//\n"
    , Comment
        ( InternalComment
            { cText = ""
            , cMultiline = False
            , cAssociationDirection = NextNode
            , cHadNewlineBefore = False
            }
        )
    )
  ,
    ( "// test"
    , Comment
        ( InternalComment
            { cText = "test"
            , cMultiline = False
            , cAssociationDirection = NextNode
            , cHadNewlineBefore = False
            }
        )
    )
  ,
    ( "[\"test\", \"test\" // cool comment \n ]"
    , mkArray
        ( fromList
            [ String "test"
            , String "test"
            , Comment
                ( InternalComment
                    { cText = "cool comment"
                    , cMultiline = False
                    , cAssociationDirection = PreviousNode
                    , cHadNewlineBefore = False
                    }
                )
            ]
        )
    )
  ]

arraySpec :: [(String, Node)]
arraySpec =
  [
    ( "[1,2,3]"
    , mkArray
        ( fromList
            [ Number (mkNumberValue "1" 1)
            , Number (mkNumberValue "2" 2)
            , Number (mkNumberValue "3" 3)
            ]
        )
    )
  ,
    ( "[1\n 2\n 3]"
    , mkArray
        ( fromList
            [ Number (mkNumberValue "1" 1)
            , Number (mkNumberValue "2" 2)
            , Number (mkNumberValue "3" 3)
            ]
        )
    )
  ]

objectSpec :: [(String, Node)]
objectSpec =
  [
    ( "{}"
    , mkObject
        (fromList [])
    )
  ,
    ( "{ }"
    , mkObject
        (fromList [])
    )
  ,
    ( "{\n//test\n\"test\" : 1, \"test2\" : 2}"
    , mkObject
        ( fromList
            [ Comment (InternalComment "test" False NextNode False)
            , ObjectKey (String "test", Number (mkNumberValue "1" 1))
            , ObjectKey (String "test2", Number (mkNumberValue "2" 2))
            ]
        )
    )
  ,
    ( "{\"test\" : 1, \"test2\" : 2}"
    , mkObject
        ( fromList
            [ ObjectKey (String "test", Number (mkNumberValue "1" 1))
            , ObjectKey (String "test2", Number (mkNumberValue "2" 2))
            ]
        )
    )
  ,
    ( "{\"test\" : 1\n \"test2\" : 2}"
    , mkObject
        ( fromList
            [ ObjectKey (String "test", Number (mkNumberValue "1" 1))
            , ObjectKey (String "test2", Number (mkNumberValue "2" 2))
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
      `shouldFailWith` err 1 (utok (toWord8 ' ') <> elabel "digit" <> elabel "integer")

topNodeSpec :: FilePath -> FilePath -> Spec
topNodeSpec inFilename outFilename = do
  let inputPath = "examples/jbeam/" ++ inFilename
  input <- runIO $ BS.readFile inputPath
  output <- runIO $ readFile outFilename
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
parseNodesState' parser = parseNodesState parser . textToLazyByteString

applyParserSpec :: (Eq a, Show a) => JbeamParser a -> (String, a) -> Spec
applyParserSpec parser = uncurry $ applySpecOnInput descFun assertParsesTo
  where
    assertParsesTo input = shouldParse (parseNodesState' parser input)
    descFun input expResult = "should parse " <> input <> " to " <> expResult

invalidTopNodeSpec :: Spec
invalidTopNodeSpec = do
  let inputPath = "examples/invalid_jbeam/invalid_fender.jbeam"
  input <- runIO $ BS.readFile inputPath
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
