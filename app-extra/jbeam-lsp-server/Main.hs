module Main (
  main,
) where

import Server (runServer)

import Formatting.Config qualified as FmtCfg

main :: IO ()
main = do
  rs <- FmtCfg.readFormattingConfig
  void (runServer rs)
