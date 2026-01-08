{-# LANGUAGE TypeFamilies #-}

module JbeamEdit.Core.NodePath (
  NodePath (..),
  NodeSelector (..),
  queryNodes,
  select,
  expectArray,
) where

import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as T (show)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.IsList (IsList (..))
import JbeamEdit.Core.Node qualified as N (
  Node (..),
  expectArray,
  isCommentNode,
  maybeObjectKey,
 )

data NodeSelector
  = ArrayIndex Int
  | ObjectKey Text
  | ObjectIndex Int
  deriving (Eq, Ord, Read, Show)

{- | node path
A NodePath is a Sequence of selectors to that point out a certain point in a Node tree, either to point at as something when fetching it from Node or to point to something compare that I at a certain point when doing updates.
-}
newtype NodePath
  = NodePath (Seq NodeSelector)
  deriving newtype (Read, Show)

instance IsList NodePath where
  type Item NodePath = NodeSelector
  fromList = NodePath . fromList
  toList (NodePath xs) = toList xs

expectArray :: NodePath -> N.Node -> Either Text (Vector N.Node)
expectArray np =
  maybe (Left $ "Expected to find array at " <> T.show np) Right . N.expectArray

extractValInKey :: N.Node -> Maybe N.Node
extractValInKey (N.ObjectKey (_, val)) = Just val
extractValInKey _ = Nothing

getNthNonComment
  :: Int -> Vector N.Node -> Maybe N.Node
getNthNonComment index nodes =
  let nonComments = V.filter (not . N.isCommentNode) nodes
   in nonComments V.!? index

{- | select
Takes a Node and a selector.
In case the selector matches nodes at a certain point in the tree.
And queryNodes allows use to chain the Selectors as a NodePath and perform complex queries.
-}
select :: NodeSelector -> N.Node -> Maybe N.Node
select (ArrayIndex i) (N.Array ns) = getNthNonComment i ns
select (ObjectKey k) (N.Object ns) = extractValInKey =<< V.find (elem k . N.maybeObjectKey) ns
select (ObjectIndex i) (N.Object a) = extractValInKey =<< getNthNonComment i a
select _ _ = Nothing

queryNodes :: NodePath -> N.Node -> Either Text N.Node
queryNodes np = maybe (Left $ "could not find " <> T.show np) Right . go np
  where
    go (NodePath (s :<| p)) n = go (NodePath p) =<< select s n
    go (NodePath Empty) n = Just n
