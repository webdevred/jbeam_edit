module CommandLineOptions (
  parseOptions,
  Options (..),
) where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Version (showVersion)
import JbeamEdit.Formatting.Config (ConfigType (..))
import Paths_jbeam_edit (version)
import System.Console.GetOpt
import System.Environment
import System.Exit (exitSuccess)

#ifdef ENABLE_TRANSFORMATION
import Data.Text qualified as T
#endif

data Options = Options
  { optInPlace :: Bool
  , optCopyJbflConfig :: Maybe ConfigType
  , optInputFile :: Maybe FilePath
  , optUpdateNames :: Map Text Text
  , optTransformation :: Bool
  }
  deriving (Show)

startOptions :: Options
startOptions =
  Options
    { optInPlace = False
    , optInputFile = Nothing
    , optCopyJbflConfig = Nothing
    , optUpdateNames = M.empty
    , optTransformation = False
    }

parseOptions :: [String] -> IO Options
parseOptions args = do
  let (actions, nonOptions, _) = getOpt RequireOrder options args
  opts <- foldr (=<<) (pure startOptions) actions
  case (optInputFile opts, nonOptions) of
    (Nothing, filename : _) -> pure $ opts {optInputFile = Just filename}
    (_, _) -> pure opts

maybeConfigType :: Maybe String -> Maybe ConfigType
maybeConfigType (Just "complex") = Just ComplexConfig
maybeConfigType (Just "minimal") = Just MinimalConfig
maybeConfigType _ = Nothing

#ifdef ENABLE_TRANSFORMATION
updateNamesOption
  :: [OptDescr (Options -> IO Options)]
updateNamesOption =
  [ Option
    "n"
    ["update-names"]
    ( ReqArg
        (\names opt -> pure opt {optUpdateNames = maybeNamesToUpdate names})
        "ORIGINAL_VERT_PREFIX:NEW_VERT_PREFIX,..."
    )
    "Update vertex names"
    , Option
    "t"
    ["transform"]
    ( NoArg
        (\opt -> pure opt {optTransformation = True})
    )
    "Enable transformation"
  ]

splitNames :: Text -> Maybe (Text, Text)
splitNames namePair =
  case T.split (== ':') namePair of
    [orig, new] -> Just (orig, new)
    _ -> Nothing

maybeNamesToUpdate :: String -> Map Text Text
maybeNamesToUpdate names =
  let namesList = T.split (== ',') (T.pack names)
   in maybe M.empty M.fromList (mapM splitNames namesList)
#else
updateNamesOption
  :: [OptDescr (Options -> IO Options)]
updateNamesOption = []
#endif

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
  ]
    ++ updateNamesOption
    ++ [ Option
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
