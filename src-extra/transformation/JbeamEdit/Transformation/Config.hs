{-# LANGUAGE DeriveGeneric #-}

module JbeamEdit.Transformation.Config (
  loadTransformationConfig,
  decodeConfig,
  transformationConfigFile,
  applyOperator,
  newTransformationConfig,
  TransformationConfig (..),
  XGroupBreakpoint (..),
  XGroupBreakpoints (..),
  defaultSortingThreshold,
  defaultSortAxes,
  defaultSupportThreshold,
  defaultBreakpoints,
  defaultMaxSupportCoordinates,
) where

import Control.Monad (forM, when)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.Functor (($>))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml (
  Object,
  ParseException (..),
  Parser,
  Value (..),
  decodeEither',
  prettyPrintParseException,
 )
import Data.Yaml.Aeson (
  FromJSON (..),
  withArray,
  withObject,
  withText,
  (.!=),
  (.:),
  (.:?),
 )
import GHC.Generics
import GHC.IO.Exception (IOErrorType (NoSuchThing))
import GHC.IsList
import JbeamEdit.IOUtils
import JbeamEdit.Transformation.Types (
  Axis (..),
  SortAxes (..),
  VertexTreeType (..),
 )
import Numeric.Natural (Natural)
import System.OsPath
import Text.Read (readMaybe)

defaultXSortingThreshold :: Maybe Scientific
defaultXSortingThreshold = Nothing

defaultSortingThreshold :: Scientific
defaultSortingThreshold = 0.05

defaultSortAxes :: SortAxes
defaultSortAxes = SortAxes AxisY AxisZ AxisX

defaultSupportThreshold :: Scientific
defaultSupportThreshold = 96

defaultMaxSupportCoordinates :: Natural
defaultMaxSupportCoordinates = 3

defaultBreakpoints :: XGroupBreakpoints
defaultBreakpoints =
  XGroupBreakpoints
    [ (XGroupBreakpoint OpGE 0.09, LeftTree) -- x >= 0.09 → LeftTree
    , (XGroupBreakpoint OpLE (-0.09), RightTree) -- x <= -0.09 → RightTree
    , (XGroupBreakpoint OpLT 0.09, MiddleTree) -- -0.09 < x < 0.09 → MiddleTree
    ]

data TransformationConfig = TransformationConfig
  { ySortingThreshold :: Scientific
  , sortAxes :: SortAxes
  , xSortingThreshold :: Maybe Scientific
  , xGroupBreakpoints :: XGroupBreakpoints
  , supportThreshold :: Scientific
  , maxSupportCoordinates :: Natural
  }
  deriving (Generic)

newTransformationConfig :: TransformationConfig
newTransformationConfig =
  TransformationConfig
    defaultSortingThreshold
    defaultSortAxes
    defaultXSortingThreshold
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
            case readMaybe (T.unpack $ T.strip rest) of
              Nothing -> fail "Invalid number"
              Just brk -> pure $ XGroupBreakpoint opFunc brk

newtype XGroupBreakpoints
  = XGroupBreakpoints
      [(XGroupBreakpoint, VertexTreeType)]
  deriving stock (Show)

instance FromJSON XGroupBreakpoints where
  parseJSON = withArray "XGroupBreakpoints" $ \arr -> do
    lst <-
      forM (toList arr) $
        withObject
          "XGroupBreakpointEntry"
          ( \o ->
              do
                bp <- o .: "breakpoint"
                vt <- o .: "vertex-type"
                pure (bp, vt)
          )
    pure $ XGroupBreakpoints lst

parseSupportThreshold :: Object -> Parser Scientific
parseSupportThreshold o = do
  thr <- o .: "support-threshold"
  when (thr < 1) failWithMessage $> thr
  where
    failWithMessage =
      fail
        "'support-threshold' must be a percentage value of 1 or higher (e.g., 80 or 80.8). Values below 1 (e.g., 0.80) are not allowed."

{- | Unquoted 'off' reaches this as a boolean, because that is how YAML
resolves it, so the string case alone would only catch the quoted spelling.
-}
parseXSortingThreshold :: Object -> Parser (Maybe Scientific)
parseXSortingThreshold o = do
  thr <- o .:? "x-sorting-threshold"
  case thr of
    Nothing -> pure defaultXSortingThreshold
    Just Null -> pure defaultXSortingThreshold
    Just (Bool False) -> pure defaultXSortingThreshold
    Just (String "off") -> pure defaultXSortingThreshold
    Just (Number number) -> pure (Just number)
    Just _ -> failWithMessage
  where
    failWithMessage =
      fail
        "'x-sorting-threshold' must be a distance in meters (e.g., 0.2 for 20 cm), or 'off' to leave the column sorting out. Omitting the key does the same as 'off'."

instance FromJSON TransformationConfig where
  parseJSON = withObject "TransformationConfig" $ \o ->
    TransformationConfig
      <$> o .:? "y-sorting-threshold" .!= defaultSortingThreshold
      -- No key reads this yet, see #243.
      <*> pure defaultSortAxes
      <*> parseXSortingThreshold o
      <*> o .:? "x-group-breakpoints" .!= defaultBreakpoints
      <*> parseSupportThreshold o
      <*> o .:? "max-support-coordinates" .!= defaultMaxSupportCoordinates

formatParseError :: ParseException -> String
formatParseError (AesonException err) = err
formatParseError excp = prettyPrintParseException excp

transformationConfigFile :: OsPath
transformationConfigFile = unsafeEncodeUtf ".jbeam-edit.yaml"

decodeConfig :: LBS.ByteString -> Either Text TransformationConfig
decodeConfig "" = Right newTransformationConfig
decodeConfig content =
  first
    (T.pack . formatParseError)
    (decodeEither' $ LBS.toStrict content)

loadTransformationConfig :: OsPath -> IO TransformationConfig
loadTransformationConfig filename = do
  content <- tryReadFile [NoSuchThing] filename
  case content >>= decodeConfig of
    Right config -> pure config
    Left err -> putErrorLine err $> newTransformationConfig
