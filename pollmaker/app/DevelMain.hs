module DevelMain where

import Application (getApplicationDev)

import Control.Exception (finally)
import Control.Concurrent
import Data.IORef
import Foreign.Store
import Network.Wai.Handler.Warp

update :: IO ()
update = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
      -- no server running
      Nothing -> do
          done <- storeAction doneStore newEmptyMVar
          tid <- start done
          _ <- storeAction (Store tidStoreNum) (newIORef tid)
          return ()
      -- server is already running
      Just tidStore ->
          -- shut the server down with killThread and wait for the done signal
          modifyStoredIORef tidStore $ \tid -> do
              killThread tid
              withStore doneStore takeMVar >> readStore doneStore >>= start
  where
    doneStore = Store 0
    tidStoreNum = 1

    modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
    modifyStoredIORef store f = withStore store $ \ref -> do
        v <- readIORef ref
        f v >>= writeIORef ref

-- | Start the server in a separate thread.
start :: MVar () -- ^ Written to when the thread is killed.
      -> IO ThreadId
start done = do
    (settings,app) <- getApplicationDev
    forkIO (finally (runSettings settings app)
                    (putMVar done ()))
