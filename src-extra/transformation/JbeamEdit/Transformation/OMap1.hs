{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module JbeamEdit.Transformation.OMap1 (
  OMap1,
  fromNEList,
  lookup,
  assocs,
  consOMap,
  singleton,
  head,
  snoc,
  uncons,
) where

import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE (uncons)
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap
import Data.Maybe (isJust)
import GHC.IsList
import Prelude hiding (head, lookup)

data OMap1 k v = OMap1 (k, v) (OMap k v)
  deriving stock (Eq, Foldable, Functor, Traversable)

instance (Ord k, Show k, Show v) => Show (OMap1 k v) where
  show (OMap1 (firstK, firstV) rest) = "OMap1 (fromList " <> show ((firstK, firstV) : OMap.assocs rest) <> ")"

instance Ord k => IsList (OMap1 k v) where
  type Item (OMap1 k v) = (k, v)
  toList (OMap1 (firstK, firstV) rest) = (firstK, firstV) : OMap.assocs rest
  fromList ((firstK, firstV) : rest)
    | firstK `elem` map fst rest =
        OMap1
          (firstK, firstV)
          (OMap.fromList $ filter (\a -> fst a /= firstK) rest)
    | otherwise = OMap1 (firstK, firstV) (OMap.fromList rest)
  fromList _ = error "OMap1: cannot convert a empty list to a OMap1."

singleton :: (k, v) -> OMap1 k v
singleton (k, v) = OMap1 (k, v) OMap.empty

lookup :: Ord k => k -> OMap1 k v -> Maybe v
lookup k (OMap1 (firstK, firstV) rest)
  | k == firstK = Just firstV
  | isJust maybeValue = maybeValue
  | otherwise = Nothing
  where
    maybeValue = OMap.lookup k rest

assocs :: OMap1 k v -> [(k, v)]
assocs (OMap1 (firstK, firstV) rest) = (firstK, firstV) : OMap.assocs rest

fromNEList :: Ord k => NonEmpty (k, v) -> OMap1 k v
fromNEList ne
  | firstK `elem` map fst (concatMap toList rest) =
      let rest' = concatMap toList rest
          Just (firstK', firstV') = find ((==) firstK . fst) $ reverse rest'
       in OMap1
            (firstK', firstV')
            (OMap.fromList $ filter ((/=) firstK . fst) rest')
  | otherwise = OMap1 (firstK, firstV) (OMap.fromList $ concatMap toList rest)
  where
    ((firstK, firstV), rest) = NE.uncons ne

head :: OMap1 k v -> v
head (OMap1 (_, firstV) _) = firstV

uncons :: Ord k => OMap1 k v -> (k, v, OMap k v)
uncons (OMap1 (firstK, firstV) rest) =
  case OMap.elemAt rest 0 of
    Just newFirst@(newFirstK, _) -> (firstK, firstV, newFirst OMap.<| OMap.delete newFirstK rest)
    Nothing -> (firstK, firstV, OMap.empty)

snoc :: Ord k => k -> v -> OMap1 k v -> OMap1 k v
snoc newLastK newLastV (OMap1 oldFirst rest)
  | fst oldFirst == newLastK = OMap1 (newLastK, newLastV) rest
  | otherwise = OMap1 oldFirst (rest OMap.>| (newLastK, newLastV))

consOMap :: k -> v -> OMap k v -> OMap1 k v
consOMap k v = OMap1 (k,v)
