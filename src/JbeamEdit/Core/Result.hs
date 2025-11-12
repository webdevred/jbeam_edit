module JbeamEdit.Core.Result (Result (..), mapResult) where

data Result bad good = Empty | Bad bad | Good good

mapResult
  :: Foldable t
  => (b -> Result a1 a2) -> t b -> ([a1], [a2])
mapResult f = foldr f' ([], [])
  where
    f' val (bad, good) =
      case f val of
        Empty -> (bad, good)
        Bad newBad -> (newBad : bad, good)
        Good newGood -> (bad, newGood : good)
