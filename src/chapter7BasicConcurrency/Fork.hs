module Chapter7BasicConcurrency.Fork where

import Control.Concurrent
import Control.Monad
import System.IO

main = do
  _ <- hSetBuffering stdout NoBuffering
  _ <- forkIO (replicateM_ 1000 (putChar 'A'))
  replicateM_ 1000 (putChar 'B')
