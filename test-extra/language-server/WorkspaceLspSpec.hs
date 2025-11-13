module WorkspaceLspSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T (pack)
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Test
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO qualified as IO (readFile)
import Test.Hspec

exampleJbflFilepath :: FilePath -> FilePath
exampleJbflFilepath cwd =
  cwd
    </> "examples"
    </> "jbfl"
    </> "minimal.jbfl"

spec :: Spec
spec = do
  cwd <- runIO getCurrentDirectory
  workspaceSpec cwd

workspaceSpec :: FilePath -> Spec
workspaceSpec cwd =
  describe "JBeam LSP Formatter"
    . it "formats a single JBeam file with a single JBFL rule correctly"
    $ ( do
          runSession
            ("jbeam-lsp-server --rules-path=" <> exampleJbflFilepath cwd)
            fullLatestClientCaps
            "examples"
            $ do
              let jbeamFile = "jbeam" </> "fender.jbeam"
                  expectedFile = "examples" </> "formatted_jbeam" </> "fender-minimal-jbfl.jbeam"

              doc <- openDoc jbeamFile "jbeam"

              formatDoc doc (LSP.FormattingOptions 0 False Nothing Nothing Nothing)

              formatted <- documentContents doc
              expected <- liftIO (T.pack <$> IO.readFile expectedFile)

              liftIO $ formatted `shouldBe` expected
      )
