module NodeCursor
  ( NodeCursor(..)
  , NodeBreadcrumb(..)
  , applyCrumb
  , comparePathAndCursor
  , newCursor
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.IsList (toList)
import NodePath qualified as NP
import Parsing (Node(..))

data NodeBreadcrumb
  = ArrayIndex Int
  | ObjectIndexAndKey (Int, Text)

instance Show NodeBreadcrumb where
  show (ArrayIndex i) = "[" <> show i <> "]"
  show (ObjectIndexAndKey (_, k)) = "." <> T.unpack k

newtype NodeCursor =
  NodeCursor [NodeBreadcrumb]

instance Show NodeCursor where
  show (NodeCursor xs) = concatMap show xs

newCursor :: NodeCursor
newCursor = NodeCursor []

type CursorFun = NodeCursor -> Node -> Node

applyCrumb :: NodeBreadcrumb -> NodeCursor -> CursorFun -> Node -> Node
applyCrumb b (NodeCursor bs) f = f (NodeCursor $ b : bs)

type SelCrumbCompFun = NP.NodeSelector -> NodeBreadcrumb -> Bool

compareSB :: SelCrumbCompFun
compareSB (NP.ObjectKey s) (ObjectIndexAndKey (_, k)) = s == k
compareSB (NP.ObjectIndex s) (ObjectIndexAndKey (i, _)) = s == i
compareSB (NP.ArrayIndex s) (ArrayIndex i) = s == i
compareSB _ _ = False

comparePathAndCursor :: NP.NodePath -> NodeCursor -> Bool
comparePathAndCursor p (NodeCursor c) = sameBy compareSB (toList p) (reverse c)

sameBy :: SelCrumbCompFun -> [NP.NodeSelector] -> [NodeBreadcrumb] -> Bool
sameBy f = go
  where
    go (a:as) (b:bs) = f a b && go as bs
    go [] [] = True
    go _ _ = False
