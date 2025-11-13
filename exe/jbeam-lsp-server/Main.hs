module Main (
  main,
) where

import CommandLineOptions
import Data.Functor (void)
import JbeamEdit.Formatting.Config qualified as FmtCfg
import JbeamEdit.LSP.Server (runServer)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOptions args
  rs <- FmtCfg.readFormattingConfig (optRulesFile opts)
  void (runServer rs)
