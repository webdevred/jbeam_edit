{-# LANGUAGE TypeFamilies #-}

module NodePath
  ( NodePath(..)
  , NodeSelector(..)
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.IsList (IsList(..))

data NodeSelector
  = ArrayIndex Int
  | ObjectKey Text
  | ObjectIndex Int

instance Show NodeSelector where
  show (ArrayIndex i) = "[" <> show i <> "]"
  show (ObjectKey k) = "." <> T.unpack k
  show (ObjectIndex k) = "." <> show k

newtype NodePath =
  NodePath [NodeSelector]

instance Show NodePath where
  show (NodePath xs) = concatMap show xs

instance IsList NodePath where
  type Item NodePath = NodeSelector
  fromList = NodePath
  toList (NodePath xs) = xs
