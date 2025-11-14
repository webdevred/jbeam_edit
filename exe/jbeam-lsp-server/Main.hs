module Main (
  main,
) where

import CommandLineOptions
import Data.Functor (void)
import Formatting.Config qualified as FmtCfg
import Server (runServer)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOptions args
  rs <- FmtCfg.readFormattingConfig
  void (runServer rs)
