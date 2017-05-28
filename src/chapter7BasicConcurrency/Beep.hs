module Chapter7BasicConcurrency.Beep where

import Control.Concurrent
import Control.Monad
import System.IO

-- main = do
--   _ <- putStrLn "foo"
--   _ <- putStrLn "foo"
--   _ <- putStrLn "foo"
--   _ <- putStrLn "foo"
--   putStrLn "bar"

-- main = forever $ do
--   s <- getLine
--   forkIO $ setReminder s

-- forkIO :: IO () -> IO ThreadId

main = loop
  where
    loop = do
      s <- getLine
      if s == "exit"
        then return ()
        else do
        _ <- forkIO $ setReminder s
        loop

setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  _ <- putStrLn $ "Ok, I'll remind you in "++(show t)++" seconds"
  _ <- threadDelay (10^6 * t)
  putStrLn $ (show t) ++ " seconds are up"

