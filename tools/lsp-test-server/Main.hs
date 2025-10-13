module Main (main) where

import Data.Functor (void)
import Server (runServer)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO qualified as IO (readFile)

main :: IO ()
main = do
  args <- getArgs
  jbflPath <- case args of
    (p : _) -> pure p
    [] -> do
      putStrLn "Usage: test-server <path-to-jbfl>"
      exitFailure

  ruleSet <- read <$> IO.readFile jbflPath
  void $ runServer ruleSet
