module Main (main) where

import Relude.Unsafe (read)
import Server (runServer)

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
