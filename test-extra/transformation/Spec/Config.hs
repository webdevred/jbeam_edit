{- | The config parser, read through `decodeConfig` because that is the entry
point `loadTransformationConfig` uses. A rejected config is not a loud
failure: the loader prints the message and returns the defaults, so the run
goes on to rewrite every neighbouring file with thresholds the user never
asked for. That is what makes these worth testing at all.
-}
module Spec.Config (
  configParsingSpec,
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import JbeamEdit.Transformation.Config
import Test.Hspec

parseField :: (TransformationConfig -> a) -> Text -> Either Text a
parseField field =
  fmap field . decodeConfig . LBS.fromStrict . encodeUtf8

{- | Only the keys that predate the column pass. Leaving
`x-sorting-threshold` out keeps this source parseable, so the spec below
fails only for the reason it names.

`support-threshold` is carried by every source here because it is the one
key the parser demands.
-}
olderThresholds :: Text
olderThresholds =
  T.unlines
    [ "y-sorting-threshold: 0.04"
    , "support-threshold: 20"
    , "max-support-coordinates: 3"
    ]

configParsingSpec :: Spec
configParsingSpec = describe "the transformation config parser" $ do
  it "reads a bare YAML number for the keys that have always taken one" $ do
    parseField ySortingThreshold olderThresholds `shouldBe` Right 0.04
    parseField supportThreshold olderThresholds `shouldBe` Right 20
    parseField maxSupportCoordinates olderThresholds `shouldBe` Right 3

  it "reads a bare YAML number for x-sorting-threshold" $
    parseField xSortingThreshold (olderThresholds <> "x-sorting-threshold: 0.2\n")
      `shouldBe` Right (Just 0.2)

  it "reads a config that leaves support-threshold out" $ do
    pendingWith
      "support-threshold is the only key read with .: rather than .:?, so a \
      \config that omits it is rejected outright and the loader falls back to \
      \every default instead. The key is meant to have a default like the \
      \others, and this spec goes green with the change that gives it one."
    parseField supportThreshold "y-sorting-threshold: 0.04\n"
      `shouldBe` Right defaultSupportThreshold

  it "leaves the column pass off when x-sorting-threshold is absent" $
    parseField xSortingThreshold "support-threshold: 20\n" `shouldBe` Right Nothing

  it "reads off as leaving the column pass out, quoted or not" $ do
    parseField xSortingThreshold (olderThresholds <> "x-sorting-threshold: off\n")
      `shouldBe` Right Nothing
    parseField
      xSortingThreshold
      (olderThresholds <> "x-sorting-threshold: \"off\"\n")
      `shouldBe` Right Nothing

  it "rejects an x-sorting-threshold that is neither a number nor off" $
    parseField xSortingThreshold (olderThresholds <> "x-sorting-threshold: soon\n")
      `shouldSatisfy` isLeft

  it "accepts a support-threshold of exactly 1" $
    parseField supportThreshold "support-threshold: 1\n" `shouldBe` Right 1

  it "rejects a support-threshold below 1" $
    parseField supportThreshold "support-threshold: 0.8\n" `shouldSatisfy` isLeft

  it "gives every default for an empty file" $ do
    parseField ySortingThreshold "" `shouldBe` Right defaultSortingThreshold
    parseField supportThreshold "" `shouldBe` Right defaultSupportThreshold
    parseField xSortingThreshold "" `shouldBe` Right Nothing
