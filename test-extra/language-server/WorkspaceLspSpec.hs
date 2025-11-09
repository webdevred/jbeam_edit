module WorkspaceLspSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T (pack)
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Test
import System.FilePath ((</>))
import System.IO qualified as IO (readFile)
import Test.Hspec

exampleJbflFilepath :: FilePath
exampleJbflFilepath = "examples" </> "ast" </> "jbfl" </> "minimal.hs"

spec :: Spec
spec =
  ( describe "JBeam LSP Formatter"
      . it "formats a single JBeam file with a single JBFL rule correctly"
  )
    . runSession
      ("jbeam-lsp-test-server " <> exampleJbflFilepath)
      fullLatestClientCaps
      "examples"
    $ ( do
          let jbeamFile = "jbeam" </> "fender.jbeam"
              expectedFile = "examples" </> "formatted_jbeam" </> "fender-minimal-jbfl.jbeam"

          doc <- openDoc jbeamFile "jbeam"

          formatDoc doc (LSP.FormattingOptions 0 False Nothing Nothing Nothing)

          formatted <- documentContents doc
          expected <- liftIO (T.pack <$> IO.readFile expectedFile)

          liftIO $ formatted `shouldBe` expected
      )
