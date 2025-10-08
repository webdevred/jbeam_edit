module Main (
  main,
) where

import Server (runServer)

main :: IO ()
main = void runServer
