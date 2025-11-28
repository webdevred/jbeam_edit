{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module JbeamEdit.Transformation.OMap1 (
  OMap1,
  omap1FromNEList,
  omap1Lookup,
  omap1Cons',
  omap1Singleton,
  omap1Union,
  omap1Head,
  omap1Cons,
  omap1Uncons,
  omap1Snoc,
) where

import Data.List.NonEmpty qualified as NE (uncons)
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Text.Show qualified (Show (show))

data OMap1 k v = OMap1 (k, v) (OMap k v)
  deriving stock (Eq, Foldable, Functor, Traversable)

instance (Ord k, Show k, Show v) => Show (OMap1 k v) where
  show (OMap1 (firstK, firstV) rest) = "OMap1 (fromList " <> show ((firstK, firstV) : OMap.assocs rest) <> ")"

omap1Singleton :: (k, v) -> OMap1 k v
omap1Singleton (k, v) = OMap1 (k, v) OMap.empty

omap1Lookup :: Ord k => k -> OMap1 k v -> Maybe v
omap1Lookup k (OMap1 (firstK, firstV) rest)
  | k == firstK = Just firstV
  | isJust maybeValue = maybeValue
  | otherwise = Nothing
  where
    maybeValue = OMap.lookup k rest

omap1FromNEList :: Ord k => NonEmpty (k, v) -> OMap1 k v
omap1FromNEList ne
  | firstK `elem` map fst (concatMap toList rest) =
      let rest' = concatMap toList rest
          Just (firstK', firstV') = find ((==) firstK . fst) $ reverse rest'
       in OMap1
            (firstK', firstV')
            (OMap.fromList $ filter ((/=) firstK . fst) rest')
  | otherwise = OMap1 (firstK, firstV) (OMap.fromList $ concatMap toList rest)
  where
    ((firstK, firstV), rest) = NE.uncons ne

omap1Head :: OMap1 k v -> v
omap1Head (OMap1 (_, firstV) _) = firstV

omap1Uncons :: Ord k => OMap1 k v -> (k, v, OMap k v)
omap1Uncons (OMap1 (firstK, firstV) rest) =
  case OMap.elemAt rest 0 of
    Just newFirst@(newFirstK, _) -> (firstK, firstV, newFirst OMap.<| OMap.delete newFirstK rest)
    Nothing -> (firstK, firstV, OMap.empty)

omap1Union :: Ord k => OMap1 k v -> OMap1 k v -> OMap1 k v
omap1Union a b =
  let toList1 (OMap1 kv rest) = kv : OMap.assocs rest

      mergedList = toList1 a <> toList1 b
      mergedOMap = OMap.fromList mergedList
      Just firstK@(k0, _) = OMap.elemAt mergedOMap 0
   in OMap1 firstK (OMap.delete k0 mergedOMap)

omap1Snoc :: Ord k => (k, v) -> OMap1 k v -> OMap1 k v
omap1Snoc newLast (OMap1 oldFirst rest)
  | fst oldFirst == fst newLast = OMap1 newLast rest
  | otherwise = OMap1 oldFirst (rest OMap.>| newLast)

omap1Cons' :: (k, v) -> OMap k v -> OMap1 k v
omap1Cons' = OMap1

omap1Cons :: Ord k => (k, v) -> OMap1 k v -> OMap1 k v
omap1Cons newFirst (OMap1 oldFirst rest)
  | fst oldFirst == fst newFirst = OMap1 newFirst rest
  | otherwise = OMap1 newFirst (oldFirst OMap.<| rest)
