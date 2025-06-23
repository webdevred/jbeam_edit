module CommandLineOptions (
  parseOptions,
  Options (..),
) where

import Data.Version (showVersion)
import Formatting.Config (ConfigType (..))
import Paths_jbeam_edit (version)
import System.Console.GetOpt
import System.Environment
import System.Exit

data Options = Options
  { optInPlace :: Bool
  , optCopyJbflConfig :: Maybe ConfigType
  , optInputFile :: Maybe FilePath
  }
  deriving (Show)

startOptions :: Options
startOptions =
  Options
    { optInPlace = False
    , optInputFile = Nothing
    , optCopyJbflConfig = Nothing
    }

parseOptions :: [String] -> IO Options
parseOptions args = do
  let (actions, nonOptions, _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (pure startOptions) actions
  case (optInputFile opts, nonOptions) of
    (Nothing, filename : _) -> pure $ opts {optInputFile = Just filename}
    (_, _) -> pure opts

maybeConfigType :: Maybe String -> Maybe ConfigType
maybeConfigType (Just "complex") = Just ComplexConfig
maybeConfigType (Just "minimal") = Just MinimalConfig
maybeConfigType _ = Nothing

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option
      "i"
      ["in-place"]
      (NoArg (\opt -> pure opt {optInPlace = True}))
      "Perform editing in-place"
  , Option
      "c"
      ["install-jbfl-config"]
      ( OptArg
          (\config opt -> pure opt {optCopyJbflConfig = maybeConfigType config})
          "JBFL-CONFIG"
      )
      "Copy rules file to config directory"
  , Option
      "V"
      ["version"]
      ( NoArg
          ( \_ -> do
              putStrLn ("Version " <> showVersion version)
              exitSuccess
          )
      )
      "Print version"
  , Option
      "h"
      ["help"]
      ( NoArg
          ( \_ -> do
              prg <- getProgName
              let header =
                    unlines
                      [ "Usage:"
                      , "  " ++ prg ++ " [OPTIONS] [INPUT-FILE]"
                      , ""
                      ]
              putStrLn (usageInfo header options)
              exitSuccess
          )
      )
      "Show help"
  ]
