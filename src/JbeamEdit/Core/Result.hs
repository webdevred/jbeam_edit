module JbeamEdit.Core.Result (Results (..), Result (..), mapResult) where

import Data.Sequence (Seq ((:<|)))

data Result bad good = Empty | Bad bad | Good good

data Results bad goods = Results [bad] goods

mapResult
  :: Foldable t
  => (a -> Result b g) -> t a -> Results b (Seq g)
mapResult f = foldr f' (Results mempty mempty)
  where
    f' val results@(Results bad good) =
      case f val of
        Empty -> results
        Bad newBad -> Results (newBad : bad) good
        Good newGood -> Results bad (newGood :<| good)
