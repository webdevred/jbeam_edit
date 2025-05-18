{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Formatting.Rules
  ( NodePatternSelector(..)
  , NodePattern(..)
  , SomeKey(..)
  , SomeProperty(..)
  , PropertyKey(..)
  , RuleSet(..)
  , lookupKey
  , allProperties
  , keyName
  ) where

import Control.Applicative ((<|>))
import Core.NodePath (NodeSelector(..))
import Data.Function (on)
import Data.List (find, intercalate)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S (fromList, map, toList)
import Data.Text (Text)
import Data.Text qualified as T

data NodePatternSelector
  = AnyKey
  | AnyIndex
  | Selector NodeSelector
  deriving (Eq, Ord)

instance Show NodePatternSelector where
  show (Selector ps) = show ps
  show AnyKey = ".*"
  show AnyIndex = "[*]"

newtype NodePattern =
  NodePattern [NodePatternSelector]
  deriving (Eq, Ord)

instance Show NodePattern where
  show (NodePattern pat) = concatMap show pat

data PropertyKey a where
  ComplexChildrenSameLine :: PropertyKey Bool
  PadZeros :: PropertyKey Bool
  PadAmount :: PropertyKey Int

data SomeKey =
  forall a. Show a =>
            SomeKey (PropertyKey a)

instance Eq SomeKey where
  p1 == p2 = on (==) keyName p1 p2

instance Ord SomeKey where
  compare p1 p2 = on compare keyName p1 p2

data SomeProperty =
  forall a. Show a =>
            SomeProperty (PropertyKey a) a

instance Show SomeProperty where
  show (SomeProperty key val) = T.unpack (propertyName key) <> " = " <> show val

propertyName :: PropertyKey a -> Text
propertyName ComplexChildrenSameLine = "ComplexChildrenSameLine"
propertyName PadZeros = "PadZeros"
propertyName PadAmount = "PadAmount"

keyName :: SomeKey -> Text
keyName (SomeKey key) = propertyName key

lookupKey :: Text -> [SomeKey] -> Maybe SomeKey
lookupKey txt = find (\(SomeKey k) -> propertyName k == txt)

boolProperties :: [SomeKey]
boolProperties = map SomeKey [ComplexChildrenSameLine, PadZeros]

intProperties :: [SomeKey]
intProperties = [SomeKey PadAmount]

allProperties :: [SomeKey]
allProperties = boolProperties ++ intProperties

newtype RuleSet =
  RuleSet (Map NodePattern (Map SomeKey SomeProperty))

instance Show RuleSet where
  show (RuleSet rs) = intercalate "\n" . map mapFun . M.assocs $ rs
    where
      mapFun (pat, props) = show pat <> " {\n" <> concatProps props <> "\n}"
      concatProps =
        intercalate "\n" . M.elems . M.map (\prop -> "  " <> show prop ++ ";")

getPropertyName :: SomeProperty -> Text
getPropertyName (SomeProperty key _) = propertyName key
