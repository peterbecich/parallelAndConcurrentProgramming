module Chapter7BasicConcurrency.MVars where

import Control.Concurrent
import Control.Monad
import System.IO

-- newEmptyMVar :: IO (MVar a)
-- newMVar :: a -> IO (MVar a)
-- takeMVar :: MVar a -> IO a
-- putMVar :: MVar a -> a -> IO () -- blocks if MVar already full

main = do
  m <- newEmptyMVar
  _ <- forkIO $ putMVar m 'x'
  r <- takeMVar m
  return ()

