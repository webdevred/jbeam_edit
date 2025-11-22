module CommandLineOptions (
  parseOptions,
  Options (..),
) where

import System.Console.GetOpt
import System.Environment (getProgName)
import System.Exit (exitSuccess)

newtype Options = Options
  { optRulesFile :: Maybe FilePath
  }
  deriving stock (Show)

startOptions :: Options
startOptions =
  Options
    { optRulesFile = Nothing
    }

trimQuotes :: String -> String
trimQuotes ('"' : xs) = trimQuotesEnd xs
  where
    trimQuotesEnd [] = []
    trimQuotesEnd s@[_] | last s == '"' = init s
    trimQuotesEnd s = s
trimQuotes s = s

parseOptions :: [String] -> IO Options
parseOptions args = do
  let (actions, nonOptions, _) = getOpt RequireOrder options args
  opts <- foldr (=<<) (pure startOptions) actions
  case optRulesFile opts of
    Nothing ->
      case nonOptions of
        [file] -> pure opts {optRulesFile = Just $ trimQuotes file}
        [] -> pure opts
        _ -> do
          putStrLn "Too many arguments."
          exitSuccess
    Just _ -> pure opts

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option
      "c"
      ["rules-path"]
      (ReqArg (\f opt -> pure opt {optRulesFile = Just f}) "FILE")
      "Provide jbfl rules path."
  , Option
      "h"
      ["help"]
      (NoArg (const showHelp))
      "Show help"
  ]

showHelp :: IO a
showHelp = do
  prg <- getProgName
  let header =
        unlines
          [ "Usage:"
          , "  " ++ prg ++ " --config-file FILE"
          , ""
          ]
  putStrLn (usageInfo header options)
  exitSuccess
