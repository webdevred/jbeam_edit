module Services.DocumentStore (open, update, get) where

import Control.Concurrent.MVar hiding (newMVar, readMVar)
import Language.LSP.Protocol.Types (Uri)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (get)

import Data.Map.Strict qualified as M
import Data.Text qualified as T

type DocumentStore = MVar (M.Map Uri T.Text)

{-# NOINLINE store #-}
store :: DocumentStore
store = unsafePerformIO (newMVar M.empty)

open :: Uri -> T.Text -> IO ()
open uri text = modifyMVar_ store (pure . M.insert uri text)

update :: Uri -> T.Text -> IO ()
update = open

get :: Uri -> IO (Maybe T.Text)
get uri = M.lookup uri <$> readMVar store
