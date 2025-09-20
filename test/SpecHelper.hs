module SpecHelper (
  applySpecOnInput,
  works,
  baseReadFile,
  listFilesInDir,
  module Core.Node,
  module Test.Hspec,
  DescribeFun,
  SpecFun,
) where

import Core.Node
import Data.List (isSuffixOf)
import System.Directory (getDirectoryContents)
import Test.Hspec

import System.IO qualified as IO (readFile)

type DescribeFun = (String -> String -> String)

type SpecFun t1 t2 a = (t1 -> t2 -> a)

baseReadFile :: FilePath -> IO String
baseReadFile = IO.readFile

listFilesInDir
  :: FilePath
  -> IO [String]
listFilesInDir dir =
  filter (\f -> isSuffixOf ".hs" f && isPrefixOf ".#" f)
    <$> getDirectoryContents dir

applySpecOnInput
  :: (Example a, Show t1, Show t2)
  => DescribeFun
  -> SpecFun t1 t2 a
  -> t1
  -> t2
  -> SpecWith (Arg a)
applySpecOnInput descFun spec input expResult =
  describe (descFun (show input) (show expResult)) . works $
    spec input expResult

works :: Example a => a -> SpecWith (Arg a)
works = it "works"
