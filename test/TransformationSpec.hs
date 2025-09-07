{-# LANGUAGE CPP #-}

module TransformationSpec (
  spec,
) where

import SpecHelper

#ifdef ENABLE_TRANSFORMATION_TESTS
import Data.List (isSuffixOf)
import Relude.Unsafe (read)
import System.Directory (getDirectoryContents)
import Transformation
import Formatting

topNodeSpec :: FilePath -> FilePath -> Spec
topNodeSpec inFilename outFilename = do
  let inputPath = "examples/ast/jbeam/" ++ inFilename
  input <- runIO $ baseReadFile inputPath
  output <- runIO $ baseReadFile outFilename
  let desc = "should transform AST in " ++ inFilename ++ " to Jbeam in " ++ outFilename
  describe desc . works $
    formatNode newRuleSet <$> transform (read input) `shouldBe` Right (toText output)

spec :: Spec
spec = do
  inputFiles <-
    runIO $ filter (isSuffixOf ".hs") <$> getDirectoryContents "examples/ast/jbeam"
  let outputFile inFile = "examples/transformed_jbeam/" ++ takeWhile (/= '.') inFile ++ ".jbeam"
      testInputFile inFile = topNodeSpec inFile (outputFile inFile)
  mapM_ testInputFile inputFiles
#else
spec :: Spec
spec = pass
#endif
