module JbeamEdit.Formatting.Rules.TrailingComma (TrailingComma (..)) where

data TrailingComma = Preserve | Force | None
  deriving stock (Eq, Ord, Read, Show)
