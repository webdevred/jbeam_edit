module WorkspaceLspSpec (spec) where

import Data.Text.IO qualified as T
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Test
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "JBeam LSP Formatter" $ do
  it "formats a single JBeam file with a single JBFL rule correctly" $ do
    runSession
      ("jbeam-lsp-test-server " <> ("examples" </> "ast" </> "jbfl" </> "minimal.hs"))
      fullLatestClientCaps
      "examples"
      $ do
        let jbeamFile = "jbeam" </> "fender.jbeam"
            expectedFile = "examples" </> "formatted_jbeam" </> "fender-minimal-jbfl.jbeam"

        doc <- openDoc jbeamFile "jbeam"

        formatDoc doc (LSP.FormattingOptions 0 False Nothing Nothing Nothing)

        formatted <- documentContents doc
        expected <- liftIO $ readFileText expectedFile

        liftIO $ formatted `shouldBe` expected
