module SpecHelper (
                   works
                  , module Node
                  , module Test.Hspec
                  ) where

import Node
import Test.Hspec

works :: Example a => a -> SpecWith (Arg a)
works = it "works"
