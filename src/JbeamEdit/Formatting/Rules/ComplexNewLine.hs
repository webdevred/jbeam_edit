module JbeamEdit.Formatting.Rules.ComplexNewLine (ComplexNewLine (..)) where

data ComplexNewLine = Force | None
  deriving stock (Eq, Ord, Read, Show)
