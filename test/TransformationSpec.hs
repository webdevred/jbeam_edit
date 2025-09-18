{-# LANGUAGE CPP #-}

module TransformationSpec (
  spec,
) where

import SpecHelper

#ifdef ENABLE_TRANSFORMATION_TESTS
import Relude.Unsafe (read)
import Transformation
import Formatting
import Config
import Data.Map qualified as M

topNodeSpec :: FilePath -> FilePath -> Spec
topNodeSpec inFilename outFilename = do
  let inputPath = "examples/ast/jbeam/" ++ inFilename
  input <- runIO $ baseReadFile inputPath
  output <- runIO $ baseReadFile outFilename
  let desc = "should transform AST in " ++ inFilename ++ " to Jbeam in " ++ outFilename
  describe desc . works $
    formatNode newRuleSet <$> transform M.empty newTransformationConfig (read input) `shouldBe` Right (toText output)

spec :: Spec
spec = do
  inputFiles <-
    runIO $ listFilesInDir "examples/ast/jbeam"
  let outputFile inFile = "examples/transformed_jbeam/" ++ takeWhile (/= '.') inFile ++ ".jbeam"
      testInputFile inFile = topNodeSpec inFile (outputFile inFile)
  mapM_ testInputFile inputFiles
#else
spec :: Spec
spec = pass
#endif
