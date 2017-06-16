module Chapter7BasicConcurrency.Noop where

import Control.Concurrent
import Control.Monad
import System.IO


noop :: IO ()
noop = do
  _ <- return ()
  noop

main :: IO()
main = do
  _ <- putStrLn "main thread"
  let t = 16
  forM_ [1..t] (\i -> do
                 tid <- forkIO noop
                 putStrLn $ "thread ID: "++(show tid)
             )

  _ <- threadDelay (30 * 10^6)
  putStrLn "stop"
                
