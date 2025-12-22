{-# LANGUAGE DeriveGeneric #-}

module JbeamEdit.Transformation.Config (
  loadTransformationConfig,
  transformationConfigFile,
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

import Control.Monad (forM, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
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
import JbeamEdit.Transformation.Types (VertexTreeType (..))
import System.OsPath
import Text.Read

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

parseSupportThreshold :: Object -> Parser Double
parseSupportThreshold o = do
  thr <- o .: "support-threshold"
  when (thr < 1) failWithMessage $> thr
  where
    failWithMessage =
      fail
        "'support-threshold' must be a percentage value of 1 or higher (e.g., 80 or 80.8). Values below 1 (e.g., 0.80) are not allowed."

instance FromJSON TransformationConfig where
  parseJSON = withObject "TransformationConfig" $ \o ->
    TransformationConfig
      <$> o .:? "z-sorting-threshold" .!= defaultSortingThreshold
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
  configEither <-
    runExceptT
      (ExceptT (tryReadFile [NoSuchThing] filename) >>= ExceptT . pure . decodeConfig)
  case configEither of
    Right config -> pure config
    Left err -> putErrorLine err $> newTransformationConfig
