{-# LANGUAGE CPP #-}

module VectorHelpers (groupWith1, sortBy, fold1) where

import Data.Vector qualified as V
import Data.Vector.Algorithms.Tim qualified as Tim (sortBy)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEV
import Prelude hiding (sortBy)

fold1 :: NonEmptyVector (NonEmptyVector a) -> NonEmptyVector a
fold1 v = go h t
  where
    v' = NEV.toVector v
    h = V.head v'
    t = V.tail v'

    go acc xs
      | V.null xs = acc
      | otherwise =
          let h' = V.unsafeHead xs
              t' = V.unsafeTail xs
           in acc <> go h' t'

groupWith1
  :: Eq b
  => (a -> b) -> NonEmptyVector a -> NonEmptyVector (NonEmptyVector a)
groupWith1 f =
  NEV.unsafeFromList
    . map NEV.unsafeFromVector
    . V.groupBy (on (==) f)
    . NEV.toVector

sortBy
  :: (a -> a -> Ordering)
  -> NonEmptyVector a
  -> NonEmptyVector a
sortBy f vec =
  NEV.unsafeFromVector $
    V.create
      ( do
          mvec <- V.thaw (NEV.toVector vec)
          Tim.sortBy f mvec
          pure mvec
      )
