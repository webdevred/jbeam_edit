{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module JbeamEdit.Transformation.OMap1 (OMap1, omap1Cons, omap1Uncons, omap1Snoc) where

import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import GHC.IsList qualified as IsList
import Text.Show qualified (Show (show))

data OMap1 k v = OMap1 (k, v) (OMap k v) deriving stock (Eq, Functor)

instance (Ord k, Show k, Show v) => Show (OMap1 k v) where
  show om1 = "OMap1 " <> show (IsList.toList om1)

instance Ord k => IsList (OMap1 k v) where
  type Item (OMap1 k v) = (k, v)
  toList (OMap1 (firstK, firstV) rest) = (firstK, firstV) : OMap.assocs rest
  fromList ((firstK, firstV) : rest) = OMap1 (firstK, firstV) (OMap.fromList rest)

instance Ord k => Foldable (OMap1 k) where
  foldMap f (OMap1 (firstK, firstV) rest) = foldMap f ((firstK, firstV) OMap.<| rest)

omap1Uncons :: Ord k => OMap1 k v -> (k, v, OMap1 k v)
omap1Uncons (OMap1 (firstK, firstV) rest) =
  let Just newFirst@(newFirstK, _) = OMap.elemAt rest 0
      newRest = OMap.delete newFirstK rest
   in (firstK, firstV, OMap1 newFirst newRest)

omap1Snoc :: Ord k => (k, v) -> OMap1 k v -> OMap1 k v
omap1Snoc newLast (OMap1 oldFirst rest) = OMap1 oldFirst (rest OMap.>| newLast)

omap1Cons :: Ord k => (k, v) -> OMap1 k v -> OMap1 k v
omap1Cons newFirst (OMap1 oldFirst rest) = OMap1 newFirst (oldFirst OMap.<| rest)
