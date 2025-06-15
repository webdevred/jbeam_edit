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
import Data.Vector ((!?))
import GHC.IsList (IsList (..))

import Core.Node qualified as N (Node (..))
import Data.Vector qualified as V

data NodeSelector
  = ArrayIndex Int
  | ObjectKey Text
  | ObjectIndex Int
  deriving (Eq, Ord, Show)

newtype NodePath
  = NodePath (Seq NodeSelector)
  deriving stock (Show)

instance IsList NodePath where
  type Item NodePath = NodeSelector
  fromList = NodePath . fromList
  toList (NodePath xs) = toList xs

extractValInKey :: N.Node -> Maybe N.Node
extractValInKey (N.ObjectKey (_, val)) = Just val
extractValInKey _ = Nothing

select :: NodeSelector -> N.Node -> Maybe N.Node
select (ArrayIndex i) (N.Array ns) = ns !? i
select (ObjectKey k) (N.Object ns) = extractValInKey =<< V.find compareKey ns
  where
    compareKey (N.ObjectKey (N.String keyText, _)) = keyText == k
    compareKey _ = False
select (ObjectIndex i) (N.Object a) = extractValInKey =<< a !? i
select _ _ = Nothing

queryNodes :: NodePath -> N.Node -> Maybe N.Node
queryNodes (NodePath (s :<| p)) n = queryNodes (NodePath p) =<< select s n
queryNodes (NodePath Empty) n = Just n
