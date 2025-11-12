module Main (
  main,
) where

import Data.Functor (void)
import JbeamEdit.Formatting.Config qualified as FmtCfg
import JbeamEdit.LSP.Server (runServer)

main :: IO ()
main = do
  rs <- FmtCfg.readFormattingConfig
  void (runServer rs)
