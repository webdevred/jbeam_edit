{-# LANGUAGE TypeFamilies #-}

module NodePath
  ( NodePath(..)
  , NodeSelector(..)
  , queryNodes
  , select
  ) where

import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq (empty, null)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector, (!), (!?), (//))
import Data.Vector qualified as V
import GHC.IsList (IsList(..))
import Parsing qualified as P (Node(..))

data NodeSelector
  = ArrayIndex Int
  | ObjectKey Text
  | ObjectIndex Int

instance Show NodeSelector where
  show (ArrayIndex i) = "[" <> show i <> "]"
  show (ObjectKey k) = "." <> T.unpack k
  show (ObjectIndex k) = "." <> show k

newtype NodePath =
  NodePath (Seq NodeSelector)

instance Show NodePath where
  show (NodePath (bs :|> b)) = show b <> show (NodePath bs)
  show (NodePath Empty) = ""

instance IsList NodePath where
  type Item NodePath = NodeSelector
  fromList = NodePath . fromList
  toList (NodePath xs) = toList xs

extractValInKey :: P.Node -> Maybe P.Node
extractValInKey (P.ObjectKey (_, val)) = Just val
extractValInKey _ = Nothing

select :: NodeSelector -> P.Node -> Maybe P.Node
select (ArrayIndex i) (P.Array ns) = ns !? i

select (ObjectKey k) (P.Object ns) = extractValInKey =<< V.find compareKey ns
  where
    compareKey (P.ObjectKey (P.String keyText, _)) = keyText == k
    compareKey _ = False
select (ObjectIndex i) (P.Object a) = extractValInKey =<< a !? i
select _ _ = Nothing

queryNodes :: NodePath -> P.Node -> Maybe P.Node
queryNodes (NodePath (s :<| p)) n = queryNodes (NodePath p) =<< select s n
