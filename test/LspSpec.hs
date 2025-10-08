{-# LANGUAGE CPP #-}

module LspSpec (spec) where

import Test.Hspec

#ifdef ENABLE_LSP_TESTS
import Language.LSP.Test
import Language.LSP.Protocol.Types as LSP
import qualified Data.Text.IO as T
import System.FilePath ((</>))

spec :: Spec
spec = describe "JBeam LSP Formatter (single test)" $ do
  it "formats a single JBeam file with a single JBFL rule correctly" $ do
    runSession "jbeam-lsp-server" fullLatestClientCaps "examples" $ do
      let
        jbeamFile    = "ast" </> "jbeam" </> "fender.jbeam"
        expectedFile = "formatted_jbeam" </> "fender-minimal-jbfl.jbeam"

      doc <- openDoc jbeamFile "jbeam"

      formatDoc doc (LSP.FormattingOptions 2 True Nothing Nothing Nothing)

      formatted <- documentContents doc
      expected  <- liftIO $ T.readFile expectedFile

      liftIO $ formatted `shouldBe` expected

#else
spec :: Spec
spec = pure ()
#endif
