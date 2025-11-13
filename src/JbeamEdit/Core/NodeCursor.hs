module JbeamEdit.Core.NodeCursor (
  NodeCursor (..),
  NodeBreadcrumb (..),
  applyCrumb,
  applyObjCrumb,
  compareSB,
  comparePathAndCursor,
  newCursor,
) where

import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq (empty, null)
import Data.Text (Text)
import JbeamEdit.Core.Node (Node (..))
import JbeamEdit.Core.NodePath qualified as NP

data NodeBreadcrumb
  = ArrayIndex Int
  | ObjectIndexAndKey (Int, Text)
  deriving (Eq, Show)

{- | node cursor
A NodeCursor is a Sequence of breadcrumbs as we transcend deeper into the Node tree.
A Seqeunce is good choice for NodeCursor since I need to compare the first element of the cursor and the path but when transcending the Node tree I need to append the current breadcrumb.
-}
newtype NodeCursor
  = NodeCursor (Seq NodeBreadcrumb)
  deriving stock (Eq, Show)

newCursor :: NodeCursor
newCursor = NodeCursor Seq.empty

type CursorFun a = NodeCursor -> Node -> a

{- | applyCrumb
This function takes a function f, a breadcrumb b, and a sequence of breadcrumbs wrapped in a NodeCursor, appends the the breadcrumb to the Sequence in the cursor and supplies the cursor to the function f. This is used whenever I update the Node tree to track where in the tree I am so, enabling checking whether I am updating at a certain point in the Node tree.
-}
applyCrumb :: NodeBreadcrumb -> NodeCursor -> CursorFun a -> Node -> a
applyCrumb b (NodeCursor bs) f = f (NodeCursor $ bs :|> b)

applyObjCrumb :: Node -> NodeCursor -> CursorFun a -> Node -> a
applyObjCrumb (String key) (NodeCursor (bs :|> (ArrayIndex i))) f =
  f (NodeCursor $ bs :|> ObjectIndexAndKey (i, key))
applyObjCrumb key cursor _ = error $ unwords [errMsg, show key, show cursor]
  where
    errMsg =
      "applyObjCrumb expects a String Node and NodeCursor with a index as the head"

type SelCrumbCompFun = NP.NodeSelector -> NodeBreadcrumb -> Bool

{- | compareSB
Validate whether all the selectors match the corresponding breadcrumb, returning False if either Sequence exhausts prematurely.
-}
compareSB :: SelCrumbCompFun
compareSB (NP.ObjectKey s) (ObjectIndexAndKey (_, k)) = s == k
compareSB (NP.ObjectIndex s) (ObjectIndexAndKey (i, _)) = s == i
compareSB (NP.ArrayIndex s) (ArrayIndex i) = s == i
compareSB _ _ = False

comparePathAndCursor :: NP.NodePath -> NodeCursor -> Bool
comparePathAndCursor (NP.NodePath p) (NodeCursor c) = sameBy compareSB p c

sameBy :: SelCrumbCompFun -> Seq NP.NodeSelector -> Seq NodeBreadcrumb -> Bool
sameBy f = go
  where
    go (p :<| ps) (b :<| bs) = f p b && go ps bs
    go ps bs = Seq.null ps && Seq.null bs
