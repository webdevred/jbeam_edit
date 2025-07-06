module Parsing.JbeamSpec (
  spec,
) where

import Data.List (isSuffixOf)
import Data.Vector (fromList)
import Parsing.Common.Helpers
import Parsing.Jbeam
import Parsing.ParsingTestHelpers
import SpecHelper
import System.Directory (getDirectoryContents)
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Data.ByteString qualified as BS (
  readFile,
 )

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

invalidSpec :: Spec
invalidSpec =
  describe "should fail when input is malformed" . works $
    parse nodeParser "" "[1,2,a,4]"
      `shouldFailWith` err 6 (utok (toWord8 'a') <> expLabels)
  where
    expLabels = foldMap elabel ["a valid scalar", "object", "array"]

invalidNumberSpec :: Spec
invalidNumberSpec =
  describe
    "should fail parsing Number when there is space after the negative sign"
    . works
    $ parse numberParser "" "- 0.3"
      `shouldFailWith` err 1 (utok (toWord8 ' ') <> elabel "digit")

topNodeSpec :: FilePath -> FilePath -> Spec
topNodeSpec inFilename outFilename = do
  let inputPath = "examples/jbeam/" ++ inFilename
  input <- runIO $ BS.readFile inputPath
  output <- runIO $ readFile outFilename
  let desc = "should parse contents of " ++ inFilename ++ " to AST in " ++ outFilename
  describe desc . works $
    parseNodes input `shouldBe` Right (read output)

topNodeSpecs :: Spec
topNodeSpecs = do
  inputFiles <-
    runIO $ filter (isSuffixOf ".jbeam") <$> getDirectoryContents "examples/jbeam"
  let outputFile inFile = "examples/ast/jbeam/" ++ takeWhile (/= '.') inFile ++ ".hs"
      testInputFile inFile = topNodeSpec inFile (outputFile inFile)
  mapM_ testInputFile inputFiles

spec :: Spec
spec = do
  mapM_ (applyParserSpec nodeParser) specs
  invalidSpec
  invalidNumberSpec
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
