{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Formatting.Rules (
  NodePatternSelector (..),
  NodePattern (..),
  SomeKey (..),
  SomeProperty (..),
  PropertyKey (..),
  RuleSet (..),
  lookupKey,
  allProperties,
  keyName,
  applyPadLogic,
  comparePatternAndCursor,
  noComplexNewLine,
  newRuleSet,
  findPropertiesForCursor,
) where

import Core.Node
import Core.NodePath (NodeSelector (..))
import Data.Function (on)
import Data.List (find)
import Data.Map (Map)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Type.Equality ((:~:) (Refl))

import Core.NodeCursor qualified as NC
import Data.Map qualified as M
import Data.Sequence qualified as Seq (null)
import Data.Text qualified as T

data NodePatternSelector
  = AnyObjectKey
  | AnyArrayIndex
  | Selector NodeSelector
  deriving (Eq, Ord, Show)

newtype NodePattern
  = NodePattern (Seq NodePatternSelector)
  deriving stock (Eq, Ord, Show)

data PropertyKey a where
  NoComplexNewLine :: PropertyKey Bool
  PadAmount :: PropertyKey Int
  PadDecimals :: PropertyKey Int

data SomeKey
  = forall a.
    (Eq a, Show a) =>
    SomeKey (PropertyKey a)

instance Show SomeKey where
  show (SomeKey key) = T.unpack (propertyName key)

instance Eq SomeKey where
  p1 == p2 = on (==) keyName p1 p2

eqKey :: PropertyKey a -> PropertyKey b -> Maybe (a :~: b)
eqKey PadAmount PadAmount = Just Refl
eqKey NoComplexNewLine NoComplexNewLine = Just Refl
eqKey PadDecimals PadDecimals = Just Refl
eqKey _ _ = Nothing

instance Ord SomeKey where
  compare = on compare keyName

data SomeProperty
  = forall a.
    (Eq a, Show a) =>
    SomeProperty (PropertyKey a) a

instance Show SomeProperty where
  show (SomeProperty key val) = T.unpack (propertyName key) <> " = " <> show val

instance Eq SomeProperty where
  SomeProperty k1 v1 == SomeProperty k2 v2 =
    case eqKey k1 k2 of
      Just Refl -> v1 == v2
      Nothing -> False

propertyName :: PropertyKey a -> Text
propertyName NoComplexNewLine = "NoComplexNewLine"
propertyName PadAmount = "PadAmount"
propertyName PadDecimals = "PadDecimals"

keyName :: SomeKey -> Text
keyName (SomeKey key) = propertyName key

lookupKey :: Text -> [SomeKey] -> Maybe SomeKey
lookupKey txt = find (\(SomeKey k) -> propertyName k == txt)

boolProperties :: [SomeKey]
boolProperties = [SomeKey NoComplexNewLine]

intProperties :: [SomeKey]
intProperties = map SomeKey [PadAmount, PadDecimals]

allProperties :: [SomeKey]
allProperties = boolProperties ++ intProperties

type Rule = Map SomeKey SomeProperty

newtype RuleSet
  = RuleSet (Map NodePattern Rule)

instance Show RuleSet where
  show (RuleSet rs) = intercalate "\n" . map mapFun . M.assocs $ rs
    where
      mapFun (pat, props) = show pat <> " {\n" <> concatProps props <> "\n}"
      concatProps =
        intercalate "\n" . M.elems . M.map (\prop -> "  " <> show prop ++ ";")

newRuleSet :: RuleSet
newRuleSet = RuleSet M.empty

lookupProp :: (Eq a, Show a) => PropertyKey a -> Rule -> Maybe a
lookupProp targetKey m =
  case M.lookup (SomeKey targetKey) m of
    Just (SomeProperty key val) ->
      case eqKey key targetKey of
        Just Refl -> Just val
        Nothing -> Nothing
    Nothing -> Nothing

applyDecimalPadding :: Int -> Text -> Text
applyDecimalPadding padDecimals node =
  let (int, frac) = T.breakOnEnd "." node
      paddedFrac = T.justifyLeft padDecimals '0' frac
   in if padDecimals /= 0
        then int <> paddedFrac
        else node

applyPadLogic :: (Node -> Text) -> Rule -> Node -> Text
applyPadLogic f rs n =
  let padAmount = sum $ lookupProp PadAmount rs
      padDecimals = sum $ lookupProp PadDecimals rs
      decimalPaddedText
        | isNumberNode n = applyDecimalPadding padDecimals (f n)
        | otherwise = f n
   in if not (isComplexNode n)
        then T.justifyRight padAmount ' ' decimalPaddedText
        else f n

noComplexNewLine :: RuleSet -> NC.NodeCursor -> Bool
noComplexNewLine rs cursor =
  let ps = findPropertiesForCursor cursor rs
      maybeProp = lookupProp NoComplexNewLine ps
   in (Just True == maybeProp)

comparePC :: NodePatternSelector -> NC.NodeBreadcrumb -> Bool
comparePC AnyObjectKey (NC.ObjectIndexAndKey (_, _)) = True
comparePC AnyArrayIndex (NC.ArrayIndex _) = True
comparePC (Selector s) bc = NC.compareSB s bc
comparePC _ _ = False

comparePatternAndCursor :: NodePattern -> NC.NodeCursor -> Bool
comparePatternAndCursor (NodePattern p) (NC.NodeCursor c) = sameBy comparePC p c

type SelCrumbCompFun = NodePatternSelector -> NC.NodeBreadcrumb -> Bool

sameBy
  :: SelCrumbCompFun -> Seq NodePatternSelector -> Seq NC.NodeBreadcrumb -> Bool
sameBy f = go
  where
    go (p :<| ps) (b :<| bs) =
      let res = f p b
       in res && go ps bs
    go ps _ = Seq.null ps

findPropertiesForCursor :: NC.NodeCursor -> RuleSet -> Rule
findPropertiesForCursor cursor (RuleSet rs) =
  case find patPointsToCursor (M.assocs rs) of
    Just (_, m) -> m
    Nothing -> M.empty
  where
    patPointsToCursor (pat, _) = pat `comparePatternAndCursor` cursor
