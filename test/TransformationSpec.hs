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

topNodeSpec :: RuleSet -> String -> TransformationConfig -> FilePath -> FilePath -> Spec
topNodeSpec rs cfName tfConfig inFilename outFilename = do
  let inputPath = "examples/ast/jbeam/" ++ inFilename
  input <- runIO $ baseReadFile inputPath
  output <- runIO $ baseReadFile outFilename
  let desc = "with " ++ cfName ++ ": should transform AST in " ++ inFilename ++ " to Jbeam in " ++ outFilename
      transformAndFormat =
          do
            (_, _, node) <- transform M.empty tfConfig (read input)
            Right (formatNode rs node)
  describe desc . works $ transformAndFormat `shouldBe` Right (toText output)

spec :: Spec
spec = do
  let exampleConfigPath = "examples/jbeam-edit.yaml"
  rs <- runIO $ baseReadFile "examples/ast/jbfl/minimal.hs"
  tfConfig <- runIO $ loadTransformationConfig exampleConfigPath
  inputFiles <-
    runIO $ listFilesInDir "examples/ast/jbeam"
  let outputFile cfName inFile = "examples/transformed_jbeam/" ++ takeWhile (/= '.') inFile ++ "-" ++ cfName ++ ".jbeam"
      testInputFile cfName tfConfig' inFile = topNodeSpec (read rs) cfName tfConfig' inFile (outputFile cfName inFile)
  mapM_ (testInputFile "cfg-default" newTransformationConfig) inputFiles
  mapM_ (testInputFile "cfg-example" tfConfig) inputFiles
#else
spec :: Spec
spec = pass
#endif
