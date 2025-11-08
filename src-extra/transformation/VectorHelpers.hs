module VectorHelpers (groupWith1, sortBy) where

import Data.Vector qualified as V
import Data.Vector.Algorithms.Tim qualified as Tim (sortBy)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEV
import Prelude hiding (sortBy)

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
