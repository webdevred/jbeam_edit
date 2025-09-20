{-# LANGUAGE DeriveGeneric #-}

module Config (
  loadTransformationConfig,
  newTransformationConfig,
  TransformationConfig (..),
  XGroupBreakpoint (..),
  XGroupBreakpoints (..),
  defaultSortingThreshold,
  defaultSupportThreshold,
  defaultBreakpoints,
) where

import Data.Char
import Data.Scientific (Scientific)
import Data.Yaml (decodeFileEither)
import Data.Yaml.Aeson (
  FromJSON (..),
  withArray,
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import Types (VertexTreeType (..))

import Data.Text qualified as T

defaultSortingThreshold :: Scientific
defaultSortingThreshold = 0.05

defaultSupportThreshold :: Double
defaultSupportThreshold = 96

defaultBreakpoints :: XGroupBreakpoints
defaultBreakpoints =
  XGroupBreakpoints
    [ (XGroupBreakpoint (>= 0.09), LeftTree) -- x >= 0.09 → LeftTree
    , (XGroupBreakpoint (> -0.09), MiddleTree) -- -0.09 < x < 0.09 → MiddleTree
    , (XGroupBreakpoint (<= -0.09), RightTree) -- x <= -0.09 → RightTree
    ]

data TransformationConfig = TransformationConfig
  { zSortingThreshold :: Scientific
  , xGroupBreakpoints :: XGroupBreakpoints
  , supportThreshold :: Double
  }
  deriving (Generic)

newTransformationConfig :: TransformationConfig
newTransformationConfig =
  TransformationConfig
    defaultSortingThreshold
    defaultBreakpoints
    defaultSupportThreshold

newtype XGroupBreakpoint = XGroupBreakpoint
  {passingBreakpoint :: Scientific -> Bool}

parseOperator :: Text -> Maybe (Scientific -> Scientific -> Bool)
parseOperator ">" = Just (>)
parseOperator "<" = Just (<)
parseOperator "<=" = Just (<=)
parseOperator ">=" = Just (>=)
parseOperator _ = Nothing

instance FromJSON XGroupBreakpoint where
  parseJSON = withText "XGroupBreakpoint" $ \text ->
    let (opTxt, rest) = T.span (`elem` (">=<" :: String)) text
     in case parseOperator opTxt of
          Nothing -> fail "Invalid operator"
          Just opFunc ->
            case readMaybe (toString $ T.dropWhile isSpace rest) of
              Nothing -> fail "Invalid number"
              Just brk -> pure $ XGroupBreakpoint (`opFunc` brk)

newtype XGroupBreakpoints
  = XGroupBreakpoints
      [(XGroupBreakpoint, VertexTreeType)]

instance FromJSON XGroupBreakpoints where
  parseJSON = withArray "XGroupBreakpoints" $ \arr -> do
    lst <- forM (toList arr) $ \obj ->
      withObject
        "XGroupBreakpointEntry"
        ( \o -> do
            bp <- o .: "breakpoint"
            vt <- o .: "vertex"
            pure (bp, vt)
        )
        obj
    pure $ XGroupBreakpoints lst

instance FromJSON TransformationConfig where
  parseJSON = withObject "TransformationConfig" $ \o ->
    TransformationConfig
      <$> o .:? "z-sorting-threshold" .!= defaultSortingThreshold
      <*> o .:? "x-group-breakpoints" .!= defaultBreakpoints
      <*> o .:? "support-threshold" .!= defaultSupportThreshold

loadTransformationConfig :: IO TransformationConfig
loadTransformationConfig = do
  res <- decodeFileEither ".jbeam-edit.yaml"
  pure $ case res of
    Right tc -> tc
    Left _ ->
      TransformationConfig
        defaultSortingThreshold
        defaultBreakpoints
        defaultSupportThreshold
