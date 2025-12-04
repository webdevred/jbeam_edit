module WorkspaceLspSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T (pack)
import Language.LSP.Protocol.Types as LSP
import Language.LSP.Test
import System.FilePath ((</>))
import System.IO qualified as IO (readFile)
import Test.Hspec

runJbeamSession :: FilePath -> Session a -> IO a
runJbeamSession jbflFile = runSessionWithConfig cfg cmd fullLatestClientCaps "examples"
  where
    cmd = "jbeam-lsp-server -c " <> jbflFile
    cfg =
      defaultConfig
        { logStdErr = True
        }

formatVerify :: FilePath -> FilePath -> Session ()
formatVerify jbeamFile expectedFile = do
  doc <- openDoc jbeamFile "jbeam"

  formatDoc doc (LSP.FormattingOptions 0 False Nothing Nothing Nothing)

  formatted <- documentContents doc
  expected <- liftIO (T.pack <$> IO.readFile expectedFile)

  liftIO $ formatted `shouldBe` expected

jbflTests :: [(FilePath, FilePath, FilePath)]
jbflTests =
  [
    ( "jbeam" </> "fender.jbeam"
    , "examples" </> "formatted_jbeam" </> "fender-minimal-jbfl.jbeam"
    , "examples" </> "jbfl" </> "minimal.jbfl"
    )
  ,
    ( "jbeam" </> "fender.jbeam"
    , "test-extra/language-server/data/expected-formatted-fender.jbeam"
    , "test-extra/language-server/data/custom-minimal.jbfl"
    )
  ]

workspaceSpec :: Spec
workspaceSpec =
  describe "JBeam LSP Formatter" $
    mapM_
      ( \(jbeam, expected, jbfl) ->
          it ("formats " <> jbeam <> " correctly") $
            runJbeamSession jbfl (formatVerify jbeam expected)
      )
      jbflTests

spec :: Spec
spec = workspaceSpec
