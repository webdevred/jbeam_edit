{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module JbeamEdit.Formatting.Rules (
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
  forceComplexNewLine,
  lookupIndentProperty,
  newRuleSet,
  findPropertiesForCursor,
) where

import Data.Function (on)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq (length, null)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Type.Equality ((:~:) (Refl))
import JbeamEdit.Core.Node
import JbeamEdit.Core.NodeCursor qualified as NC
import JbeamEdit.Core.NodePath (NodeSelector (..))
import Text.Read qualified as TR

data NodePatternSelector
  = AnyObjectKey
  | AnyArrayIndex
  | Selector NodeSelector
  deriving stock (Eq, Read, Show)

instance Ord NodePatternSelector where
  compare a b = compare (rank a) (rank b)
    where
      rank :: NodePatternSelector -> (Int, Maybe NodeSelector)
      rank AnyArrayIndex = (2, Nothing)
      rank AnyObjectKey = (1, Nothing)
      rank (Selector s) = (0, Just s)

newtype NodePattern
  = NodePattern (Seq NodePatternSelector)
  deriving stock (Eq, Read, Show)

instance Ord NodePattern where
  compare (NodePattern a) (NodePattern b) =
    case on compare (Down . Seq.length) a b of
      EQ -> compare a b
      c -> c

data PropertyKey a where
  NoComplexNewLine :: PropertyKey Bool
  ForceComplexNewLine :: PropertyKey Bool
  PadAmount :: PropertyKey Int
  PadDecimals :: PropertyKey Int
  Indent :: PropertyKey Int

data SomeKey
  = forall a.
    (Eq a, Read a, Show a) =>
    SomeKey (PropertyKey a)

instance Show SomeKey where
  show (SomeKey key) = "SomeKey " <> T.unpack (propertyName key)

instance Read SomeKey where
  readsPrec _ s =
    case TR.lex s of
      [("SomeKey", rest1)] ->
        case TR.lex rest1 of
          [(keyStr, rest2)] ->
            case lookupKey (T.pack keyStr) allProperties of
              Just theKey -> [(theKey, rest2)]
              Nothing -> error ("invalid key: " ++ keyStr)
          _ -> []
      _ -> []

instance Eq SomeKey where
  p1 == p2 = on (==) keyName p1 p2

eqKey :: PropertyKey a -> PropertyKey b -> Maybe (a :~: b)
eqKey PadAmount PadAmount = Just Refl
eqKey NoComplexNewLine NoComplexNewLine = Just Refl
eqKey ForceComplexNewLine ForceComplexNewLine = Just Refl
eqKey PadDecimals PadDecimals = Just Refl
eqKey Indent Indent = Just Refl
eqKey _ _ = Nothing

instance Ord SomeKey where
  compare = on compare keyName

data SomeProperty
  = forall a.
    (Eq a, Show a) =>
    SomeProperty (PropertyKey a) a

instance Show SomeProperty where
  show (SomeProperty key val) = "SomeProperty " ++ T.unpack (propertyName key) ++ " " ++ show val

instance Read SomeProperty where
  readsPrec _ s =
    case TR.lex s of
      [("SomeProperty", rest1)] ->
        case TR.lex rest1 of
          (keyStr, rest2) : _ ->
            case lookupKey (T.pack keyStr) allProperties of
              Just (SomeKey (key :: PropertyKey a)) ->
                case reads rest2 of
                  [(val, rest3)] -> [(SomeProperty key val, rest3)]
                  _ -> []
              Nothing -> []
          _ -> []
      _ -> []

instance Eq SomeProperty where
  SomeProperty k1 v1 == SomeProperty k2 v2 =
    case eqKey k1 k2 of
      Just Refl -> v1 == v2
      Nothing -> False

propertyName :: PropertyKey a -> Text
propertyName NoComplexNewLine = "NoComplexNewLine"
propertyName ForceComplexNewLine = "ForceComplexNewLine"
propertyName PadAmount = "PadAmount"
propertyName PadDecimals = "PadDecimals"
propertyName Indent = "Indent"

keyName :: SomeKey -> Text
keyName (SomeKey key) = propertyName key

lookupKey :: Text -> [SomeKey] -> Maybe SomeKey
lookupKey txt = find (\(SomeKey k) -> propertyName k == txt)

boolProperties :: [SomeKey]
boolProperties = map SomeKey [ForceComplexNewLine, NoComplexNewLine]

intProperties :: [SomeKey]
intProperties = map SomeKey [PadAmount, PadDecimals, Indent]

allProperties :: [SomeKey]
allProperties = boolProperties ++ intProperties

type Rule = Map SomeKey SomeProperty

newtype RuleSet
  = RuleSet (Map NodePattern Rule)
  deriving stock (Eq, Read, Show)

newRuleSet :: RuleSet
newRuleSet = RuleSet M.empty

lookupProp :: (Eq a, Read a, Show a) => PropertyKey a -> Rule -> Maybe a
lookupProp targetKey m =
  case M.lookup (SomeKey targetKey) m of
    Just (SomeProperty key val) ->
      case eqKey key targetKey of
        Just Refl -> Just val
        Nothing -> Nothing
    Nothing -> Nothing

applyDecimalPadding :: Int -> Text -> Text
applyDecimalPadding padDecimals node
  | padDecimals /= 0 =
      let (int, frac) = T.breakOnEnd "." node
          paddedFrac = T.justifyLeft padDecimals '0' frac
       in int <> paddedFrac
  | T.isSuffixOf ".0" node = T.dropEnd 2 node
  | otherwise = node

applyPadLogic :: (Node -> Text) -> Rule -> Node -> Text
applyPadLogic f rs n =
  let padAmount = sum $ lookupProp PadAmount rs
      padDecimals = sum $ lookupProp PadDecimals rs
      decimalPaddedText
        | isNumberNode n = applyDecimalPadding padDecimals (f n)
        | otherwise = f n
   in if not (isComplexNode n)
        then T.justifyLeft padAmount ' ' decimalPaddedText
        else f n

forceComplexNewLine :: RuleSet -> NC.NodeCursor -> Bool
forceComplexNewLine rs cursor =
  let ps = findPropertiesForCursor cursor rs
      maybeProp = lookupProp ForceComplexNewLine ps
   in (Just True == maybeProp)

noComplexNewLine :: RuleSet -> NC.NodeCursor -> Bool
noComplexNewLine rs cursor =
  let ps = findPropertiesForCursor cursor rs
      maybeProp = lookupProp NoComplexNewLine ps
   in (Just True == maybeProp)

lookupIndentProperty :: RuleSet -> NC.NodeCursor -> Int
lookupIndentProperty rs cursor =
  let ps = findPropertiesForCursor cursor rs
      indentProperty = lookupProp Indent ps
   in fromMaybe 2 indentProperty

comparePC :: NodePatternSelector -> NC.NodeBreadcrumb -> Bool
comparePC AnyObjectKey (NC.ObjectIndexAndKey _ _) = True
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
  case filter patPointsToCursor (M.assocs rs) of
    [] -> M.empty
    ms -> M.unions (map snd ms)
  where
    patPointsToCursor (pat, _) = pat `comparePatternAndCursor` cursor
