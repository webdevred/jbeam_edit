{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Formatting.RuleFormatting (
  formatRuleSet,
) where

import Core.NodePath qualified as NP (NodeSelector (..))
import Data.Map qualified as M
import Data.Text qualified as T
import Formatting.Rules

formatNodeSelector :: NP.NodeSelector -> Text
formatNodeSelector (NP.ArrayIndex i) = "[" <> show i <> "]"
formatNodeSelector (NP.ObjectIndex i) = "." <> show i
formatNodeSelector (NP.ObjectKey k) = "." <> show k

formatNodePatternSelector :: NodePatternSelector -> Text
formatNodePatternSelector AnyArrayIndex = "[*]"
formatNodePatternSelector AnyObjectKey = ".*"
formatNodePatternSelector (Selector s) = formatNodeSelector s

formatNodePattern :: NodePattern -> Text
formatNodePattern (NodePattern ps) = foldMap formatNodePatternSelector ps

formatProperty :: SomeKey -> SomeProperty -> Text
formatProperty _ (SomeProperty key val) = "  " <> propertyName key <> " : " <> show val <> ";\n"

formatRuleSet :: RuleSet -> Text
formatRuleSet (RuleSet rs) = T.intercalate "\n\n" (M.foldMapWithKey formatPatPropPair rs) <> "\n"
  where
    formatPatPropPair pat props = pure $ formatNodePattern pat <> "{\n" <> formatProps props <> "}"
    formatProps = M.foldMapWithKey formatProperty
