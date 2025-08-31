{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Core.NodePath (
  NodePath (..),
  NodeSelector (..),
  queryNodes,
  select,
) where

import Data.Sequence (Seq (..))
import Data.Text (Text)
import GHC.IsList (IsList (..))

import Core.Node qualified as N (Node (..))
import Data.Vector qualified as V

data NodeSelector
  = ArrayIndex Int
  | ObjectKey Text
  | ObjectIndex Int
  deriving (Eq, Ord, Read, Show)

newtype NodePath
  = NodePath (Seq NodeSelector)
  deriving stock (Read, Show)

instance IsList NodePath where
  type Item NodePath = NodeSelector
  fromList = NodePath . fromList
  toList (NodePath xs) = Prelude.toList xs

extractValInKey :: N.Node -> Maybe N.Node
extractValInKey (N.ObjectKey (_, val)) = Just val
extractValInKey _ = Nothing

select :: NodeSelector -> N.Node -> Maybe N.Node
select (ArrayIndex i) (N.Array ns) = getNthElem 0 (V.toList ns)
  where
    getNthElem _ [] = Nothing
    getNthElem curIndex ((N.Comment _) : rest) = getNthElem curIndex rest
    getNthElem curIndex (curElem : rest)
      | curIndex == i = Just curElem
      | otherwise = getNthElem (curIndex + 1) rest
select (ObjectKey k) (N.Object ns) = extractValInKey =<< V.find compareKey ns
  where
    compareKey (N.ObjectKey (N.String keyText, _)) = keyText == k
    compareKey _ = False
select (ObjectIndex i) (N.Object a) = getNthKey 0 (V.toList a)
  where
    getNthKey _ [] = Nothing
    getNthKey curIndex ((N.ObjectKey (_, value)) : rest)
      | curIndex == i = Just value
      | otherwise = getNthKey (curIndex + 1) rest
    getNthKey curIndex (_ : obj_elems) = getNthKey curIndex obj_elems
select _ _ = Nothing

queryNodes :: NodePath -> N.Node -> Maybe N.Node
queryNodes (NodePath (s :<| p)) n = queryNodes (NodePath p) =<< select s n
queryNodes (NodePath Empty) n = Just n
