module Services.DocumentStore (open, update, get, delete, resetStore) where

import Control.Concurrent.MVar hiding (newMVar, readMVar)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import IOUtils
import Language.LSP.Protocol.Types (Uri)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (get)

type DocumentStore = MVar (M.Map Uri T.Text)

{-# NOINLINE store #-}
store :: DocumentStore
store = unsafePerformIO $ do
  putErrorLine "[Info] Initializing DocumentStore"
  newMVar M.empty

resetStore :: IO ()
resetStore = modifyMVar_ store (const (pure M.empty))

open :: Uri -> T.Text -> IO ()
open uri text = modifyMVar_ store (pure . M.insert uri text)

update :: Uri -> Text -> IO ()
update = open

get :: Uri -> IO (Maybe Text)
get uri = M.lookup uri <$> readMVar store

delete :: Uri -> IO ()
delete uri = modifyMVar_ store (pure . M.delete uri)
