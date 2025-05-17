module Formatting.Rules
  (
  ) where

import Data.Char (chr, isSpace, ord)
import NodePath (NodeSelector(..))
import Parsing.Internal qualified as H
  ( Parser
  , byteChar
  , charNotEqWord8
  , failingParser
  , parseWord8s
  , skipWhiteSpace
  , toChar
  , toWord8
  , tryParsers
  )

import Control.Applicative ((<|>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Byte qualified as B
import Text.Megaparsec.Byte.Lexer qualified as L (decimal)
import Text.Megaparsec.Char qualified as C

data NodePatternSelector
  = AnyKey
  | AnyIndex
  | Selector NodeSelector
  deriving (Eq, Ord)

newtype NodePattern =
  NodePattern [NodePatternSelector]
  deriving (Eq, Ord)

keyParser :: H.Parser NodePatternSelector
keyParser = H.byteChar '.' *> key
  where
    key = H.parseWord8s (Selector . ObjectKey) (MP.some . MP.satisfy $ p)
    p w =
      let c = H.toChar w
       in not (isSpace c) && c `notElem` ['[', '.']

indexParser :: H.Parser NodePatternSelector
indexParser = H.byteChar '[' *> index <* H.byteChar ']'
  where
    index = Selector . ArrayIndex <$> L.decimal

anyIndexParser :: H.Parser NodePatternSelector
anyIndexParser = AnyIndex <$ B.string "[*]"

anyKeyParser :: H.Parser NodePatternSelector
anyKeyParser = AnyKey <$ B.string ".*"

patternSelectorParser :: H.Parser NodePatternSelector
patternSelectorParser = H.skipWhiteSpace *> anySel <* H.skipWhiteSpace
  where
    expLabels = ["\\[[0-9]+\\]", "\\.[a-zA-Z]+", "\\[*\\]", "\\.\\*"]
    anySel =
      H.tryParsers [keyParser, indexParser, anyIndexParser, anyKeyParser]
        <|> H.failingParser expLabels

patternParser = MP.some patternSelectorParser
