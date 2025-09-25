module FormattingSpec (
  spec,
) where

import Formatting
import Relude.Unsafe (read)
import SpecHelper
import System.FilePath (takeBaseName, (</>))

numberSpec :: [(String, Node)]
numberSpec =
  [ ("123.0", Number 123)
  , ("123.123", Number 123.123)
  , ("-123.0", Number (-123))
  , ("-123.123", Number (-123.123))
  , ("0.0", Number 0.0)
  ]

stringSpec :: [(String, Node)]
stringSpec = [("\"test\"", String "test"), ("\"\"", String "")]

boolSpec :: [(String, Node)]
boolSpec = [("true", Bool True), ("false", Bool False)]

nullSpec :: [(String, Node)]
nullSpec = [("null", Null)]

multilineCommentSpec :: [(String, Node)]
multilineCommentSpec = [("/* test */", Comment (InternalComment "test" True NextNode))]

singlelineCommentSpec :: [(String, Node)]
singlelineCommentSpec = [("\n// test", Comment (InternalComment "test" False NextNode))]

arraySpec :: [(String, Node)]
arraySpec =
  [ ("[1.0, 2.0, 3.0]", Array (fromList [Number 1, Number 2, Number 3]))
  ]

objectSpec :: [(String, Node)]
objectSpec =
  [
    ( "{\"test\" : 1.0, \"test2\" : 2.0}"
    , Object
        ( fromList
            [ ObjectKey (String "test", Number 1)
            , ObjectKey (String "test2", Number 2)
            ]
        )
    )
  ]

dynamicJbflTests :: IO [(Text, Text)]
dynamicJbflTests = do
  let examplesDir = "examples"
      jbeamAstDir = examplesDir </> "ast/jbeam"
      jbflAstDir = examplesDir </> "ast/jbfl"
      formattedDir = examplesDir </> "formatted_jbeam"

  jbeamFiles <- listFilesInDir jbeamAstDir
  jbflFiles <- listFilesInDir jbflAstDir

  forM [(j, b) | j <- jbeamFiles, b <- jbflFiles] $ \(jbeamFile, jbflFile) -> do
    jbeam <- read <$> baseReadFile (jbeamAstDir </> jbeamFile)
    rules <- read <$> baseReadFile (jbflAstDir </> jbflFile)

    let formatted = formatNode rules jbeam
        baseName = takeBaseName jbeamFile ++ "-" ++ takeBaseName jbflFile
        outFile = formattedDir </> (baseName ++ "-jbfl.jbeam")

    expected <- toText <$> baseReadFile outFile
    pure (formatted, expected)

spec :: Spec
spec = do
  mapM_ formatNodeSpec specs

  it "formats all JBEAM ASTs with all JBFL rules and matches dumped files" $ do
    dynamicTests <- dynamicJbflTests
    forM_ dynamicTests $ uncurry shouldBe
  where
    formatNodeSpec (jbeam, node) =
      applySpecOnInput
        descFun
        shouldBe
        (formatNode newRuleSet node)
        (fromString jbeam)
    descFun jbeam node = "should format " ++ show node ++ " as " ++ jbeam
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
