module SpecHelper
  ( applySpecOnInput
  , works
  , module Node
  , module Test.Hspec
  ) where

import Node
import Test.Hspec

type DescribeFun = (String -> String -> String)

type SpecFun t1 t2 a = (t1 -> t2 -> a)

applySpecOnInput ::
     (Show t1, Show t2, Example a)
  => DescribeFun
  -> SpecFun t1 t2 a
  -> t1
  -> t2
  -> SpecWith (Arg a)
applySpecOnInput descFun spec input expResult =
  describe (descFun (show input) (show expResult)) . works
    $ spec input expResult

works :: Example a => a -> SpecWith (Arg a)
works = it "works"
