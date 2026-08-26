{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module JbeamEdit.Formatting.Rules (
  NodePatternSelector (..),
  SomeKey (..),
  SomeProperty (..),
  PropertyKey (..),
  module JbeamEdit.Formatting.Rules.ComplexNewLine,
  module JbeamEdit.Formatting.Rules.TrailingComma,
  Rule,
  RuleSet (..),
  lookupKey,
  allProperties,
  deprecatedAliases,
  Reach (..),
  propertyReach,
  keyName,
  applyPadLogic,
  complexNewLine,
  lookupProperty,
  lookupPropertyForCursor,
  findPropertiesForCursor,
) where

import Data.Bool (bool)
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (find, sortOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ord (Down (..))
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Type.Equality ((:~:) (Refl))
import JbeamEdit.Core.Node
import JbeamEdit.Core.NodeCursor qualified as NC
import JbeamEdit.Core.NodePath qualified as NP (NodeSelector (..))
import JbeamEdit.Formatting.Rules.ComplexNewLine (ComplexNewLine)
import JbeamEdit.Formatting.Rules.ComplexNewLine qualified as CNL
import JbeamEdit.Formatting.Rules.TrailingComma (TrailingComma)
import Text.Read qualified as TR

data NodePatternSelector
  = AnyObjectKey
  | AnyArrayIndex
  | Selector NP.NodeSelector
  deriving stock (Eq, Read, Show)

instance Monoid RuleSet where
  mempty = RuleSet M.empty [] mempty mempty M.empty M.empty

instance Semigroup RuleSet where
  (RuleSet rs1 ps1 aok1 aai1 h1 b1) <> (RuleSet rs2 ps2 aok2 aai2 h2 b2) =
    RuleSet
      (M.unionWith (<>) rs1 rs2)
      (mergePrefixes ps1 ps2)
      (aok1 <> aok2)
      (aai1 <> aai2)
      (h1 <> h2)
      (b1 <> b2)

data PropertyKey a where
  AutoPad :: PropertyKey Bool
  ComplexNewLine :: PropertyKey ComplexNewLine
  AlignObjectKeys :: PropertyKey Bool
  AutoPadSubObjects :: PropertyKey Bool
  PreserveNumberFormat :: PropertyKey Bool
  PadAmount :: PropertyKey Int
  PadDecimals :: PropertyKey Int
  Indent :: PropertyKey Int
  TrailingComma :: PropertyKey TrailingComma

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
eqKey AutoPad AutoPad = Just Refl
eqKey PadAmount PadAmount = Just Refl
eqKey ComplexNewLine ComplexNewLine = Just Refl
eqKey AlignObjectKeys AlignObjectKeys = Just Refl
eqKey AutoPadSubObjects AutoPadSubObjects = Just Refl
eqKey PreserveNumberFormat PreserveNumberFormat = Just Refl
eqKey PadDecimals PadDecimals = Just Refl
eqKey Indent Indent = Just Refl
eqKey TrailingComma TrailingComma = Just Refl
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
propertyName AutoPad = "AutoPad"
propertyName ComplexNewLine = "ComplexNewLine"
propertyName AlignObjectKeys = "AlignObjectKeys"
propertyName AutoPadSubObjects = "AutoPadSubObjects"
propertyName PreserveNumberFormat = "PreserveNumberFormat"
propertyName PadAmount = "PadAmount"
propertyName PadDecimals = "PadDecimals"
propertyName Indent = "Indent"
propertyName TrailingComma = "TrailingComma"

keyName :: SomeKey -> Text
keyName (SomeKey key) = propertyName key

lookupKey :: Text -> [SomeKey] -> Maybe SomeKey
lookupKey txt = find (\(SomeKey k) -> propertyName k == txt)

boolProperties :: [SomeKey]
boolProperties =
  map
    SomeKey
    [ AutoPad
    , AlignObjectKeys
    , AutoPadSubObjects
    , PreserveNumberFormat
    ]

enumProperties :: [SomeKey]
enumProperties = [SomeKey ComplexNewLine, SomeKey TrailingComma]

intProperties :: [SomeKey]
intProperties = map SomeKey [PadAmount, PadDecimals, Indent]

allProperties :: [SomeKey]
allProperties = boolProperties ++ enumProperties ++ intProperties

mergePrefixes :: [(Text, RuleSet)] -> [(Text, RuleSet)] -> [(Text, RuleSet)]
mergePrefixes ps1 ps2 =
  sortOn (Down . T.length . fst) . M.toList . M.fromListWith (flip (<>)) $
    ps1 <> ps2

data Reach = Here | Below
  deriving stock (Eq, Show)

propertyReach :: PropertyKey a -> Reach
propertyReach AutoPad = Here
propertyReach AlignObjectKeys = Here
propertyReach AutoPadSubObjects = Here
propertyReach ComplexNewLine = Below
propertyReach PreserveNumberFormat = Below
propertyReach PadAmount = Below
propertyReach PadDecimals = Below
propertyReach Indent = Below
propertyReach TrailingComma = Below

-- | Maps deprecated property names to (key, value-when-true, value-when-false).
deprecatedAliases :: [(Text, (SomeKey, SomeProperty, SomeProperty))]
deprecatedAliases =
  [
    ( "NoComplexNewLine"
    ,
      ( SomeKey ComplexNewLine
      , SomeProperty ComplexNewLine CNL.None
      , SomeProperty ComplexNewLine CNL.Force
      )
    )
  ,
    ( "ForceComplexNewLine"
    ,
      ( SomeKey ComplexNewLine
      , SomeProperty ComplexNewLine CNL.Force
      , SomeProperty ComplexNewLine CNL.None
      )
    )
  ]

type Rule = Map SomeKey SomeProperty

data RuleSet
  = RuleSet
  { rsBySelectors :: Map NP.NodeSelector RuleSet
  , rsPrefixes :: [(Text, RuleSet)]
  , rsAnyObjectKey :: Maybe RuleSet
  , rsAnyArrayIndex :: Maybe RuleSet
  , rsHere :: Rule
  , rsBelow :: Rule
  }
  deriving stock (Eq, Read, Show)

lookupProperty :: (Eq a, Read a, Show a) => PropertyKey a -> Rule -> Maybe a
lookupProperty targetKey m =
  case M.lookup (SomeKey targetKey) m of
    Just (SomeProperty key val) ->
      case eqKey key targetKey of
        Just Refl -> Just val
        Nothing -> Nothing
    Nothing -> Nothing

applyDecimalPadding :: Int -> Text -> Text
applyDecimalPadding padDecimals node
  | padDecimals /= 0
  , T.any (== '.') node =
      let (int, frac) = T.breakOnEnd "." node
          cleanFrac = T.dropWhileEnd ('0' ==) frac
          paddedFrac = T.justifyLeft padDecimals '0' cleanFrac
       in int <> paddedFrac
  | otherwise = node

applyPadLogic :: (Node -> Text) -> Rule -> Node -> Text
applyPadLogic f rs n =
  let padAmount = sum $ lookupProperty PadAmount rs
      padDecimals = sum $ lookupProperty PadDecimals rs
      decimalPaddedText
        | isNumberNode n = applyDecimalPadding padDecimals (f n)
        | otherwise = f n
   in bool (T.justifyLeft padAmount ' ' decimalPaddedText) (f n) (isComplexNode n)

complexNewLine :: RuleSet -> NC.NodeCursor -> Maybe ComplexNewLine
complexNewLine rs cursor =
  let ps = findPropertiesForCursor cursor rs
   in lookupProperty ComplexNewLine ps

lookupPropertyForCursor
  :: (Eq a, Read a, Show a)
  => PropertyKey a -> RuleSet -> NC.NodeCursor -> Maybe a
lookupPropertyForCursor key rs cursor =
  lookupProperty key (findPropertiesForCursor cursor rs)

findPropertiesForCursor :: NC.NodeCursor -> RuleSet -> Rule
findPropertiesForCursor (NC.NodeCursor cursor) = go cursor
  where
    go Empty rs = rs.rsHere <> rs.rsBelow
    go (NC.ObjectIndexAndKey i k :<| bs) rs =
      go
        bs
        ( addBelowProps rs $
            fold (M.lookup (NP.ObjectKey k) rs.rsBySelectors)
              <> fold (M.lookup (NP.ObjectIndex i) rs.rsBySelectors)
              <> matchingPrefixes k rs.rsPrefixes
              <> fold rs.rsAnyObjectKey
        )
    go (NC.ArrayIndex i :<| bs) rs =
      go
        bs
        ( addBelowProps rs $
            fold (M.lookup (NP.ArrayIndex i) rs.rsBySelectors)
              <> fold rs.rsAnyArrayIndex
        )
    addBelowProps rsAbove rs = rs {rsBelow = rs.rsBelow <> rsAbove.rsBelow}
    matchingPrefixes k = foldMap snd . filter ((`T.isPrefixOf` k) . fst)
