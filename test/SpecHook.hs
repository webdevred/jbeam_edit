module SpecHook (hook) where

import Test.Hspec
import Test.Hspec.JUnit.Config

import Test.Hspec.JUnit.Formatter qualified as Formatter

hook :: Spec -> Spec
hook = Formatter.register $ defaultJUnitConfig "test-suite"
