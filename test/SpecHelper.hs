module SpecHelper
  ( applySpecOnInput
  , works
  , module Node
  , module Test.Hspec
  ) where

import Node
import Test.Hspec

applySpecOnInput descFun spec input expResult =
  describe (descFun (show input) (show expResult)) . works
    $ spec input expResult

works :: Example a => a -> SpecWith (Arg a)
works = it "works"
