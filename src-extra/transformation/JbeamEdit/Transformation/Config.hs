{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module JbeamEdit.Transformation.Config (
  loadTransformationConfig,
  applyOperator,
  newTransformationConfig,
  TransformationConfig (..),
  XGroupBreakpoint (..),
  XGroupBreakpoints (..),
  defaultSortingThreshold,
  defaultSupportThreshold,
  defaultBreakpoints,
  defaultMaxSupportCoordinates,
) where

import Data.Scientific (Scientific)
import Data.Text qualified as T
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
import IOUtils
import JbeamEdit.Transformation.Types (VertexTreeType (..))

defaultSortingThreshold :: Scientific
defaultSortingThreshold = 0.05

defaultSupportThreshold :: Double
defaultSupportThreshold = 96

defaultMaxSupportCoordinates :: Int
defaultMaxSupportCoordinates = 3

defaultBreakpoints :: XGroupBreakpoints
defaultBreakpoints =
  XGroupBreakpoints
    [ (XGroupBreakpoint OpGE 0.09, LeftTree) -- x >= 0.09 → LeftTree
    , (XGroupBreakpoint OpLE (-0.09), RightTree) -- x <= -0.09 → RightTree
    , (XGroupBreakpoint OpLT 0.09, MiddleTree) -- -0.09 < x < 0.09 → MiddleTree
    ]

data TransformationConfig = TransformationConfig
  { zSortingThreshold :: Scientific
  , xGroupBreakpoints :: XGroupBreakpoints
  , supportThreshold :: Double
  , maxSupportCoordinates :: Int
  }
  deriving (Generic)

newTransformationConfig :: TransformationConfig
newTransformationConfig =
  TransformationConfig
    defaultSortingThreshold
    defaultBreakpoints
    defaultSupportThreshold
    defaultMaxSupportCoordinates

data XGroupBreakpoint = XGroupBreakpoint Operator Scientific deriving (Show)

data Operator = OpLT | OpGT | OpLE | OpGE deriving (Show)

applyOperator :: Operator -> Scientific -> Scientific -> Bool
applyOperator OpLT x y = x < y
applyOperator OpGT x y = x > y
applyOperator OpLE x y = x <= y
applyOperator OpGE x y = x >= y

parseOperator :: Text -> Maybe Operator
parseOperator ">" = Just OpGT
parseOperator "<" = Just OpLT
parseOperator "<=" = Just OpLE
parseOperator ">=" = Just OpGE
parseOperator _ = Nothing

instance FromJSON XGroupBreakpoint where
  parseJSON = withText "XGroupBreakpoint" $ \text ->
    let (opTxt, rest) = T.span (`elem` (">=<" :: String)) text
     in case parseOperator opTxt of
          Nothing -> fail "Invalid operator"
          Just opFunc ->
            case readMaybe (toString $ T.strip rest) of
              Nothing -> fail "Invalid number"
              Just brk -> pure $ XGroupBreakpoint opFunc brk

newtype XGroupBreakpoints
  = XGroupBreakpoints
      [(XGroupBreakpoint, VertexTreeType)]
  deriving stock (Show)

instance FromJSON XGroupBreakpoints where
  parseJSON = withArray "XGroupBreakpoints" $ \arr -> do
    lst <- forM (toList arr) $ \obj ->
      withObject
        "XGroupBreakpointEntry"
        ( \o -> do
            bp <- o .: "breakpoint"
            vt <- o .: "vertex-type"
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
      <*> o .:? "max-support-coordinates" .!= defaultMaxSupportCoordinates

loadTransformationConfig :: FilePath -> IO TransformationConfig
loadTransformationConfig filename = do
  res <- decodeFileEither filename
  case res of
    Right tc -> pure tc
    Left err -> do
      putErrorLine $ show err
      pure
        ( TransformationConfig
            defaultSortingThreshold
            defaultBreakpoints
            defaultSupportThreshold
            defaultMaxSupportCoordinates
        )
