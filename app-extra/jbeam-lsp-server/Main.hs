module Main (
  main,
) where

import Formatting.Config qualified as FmtCfg
import Server (runServer)

main :: IO ()
main = do
  rs <- FmtCfg.readFormattingConfig
  void (runServer rs)
