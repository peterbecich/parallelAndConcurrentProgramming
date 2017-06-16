module Chapter7BasicConcurrency.Logging where

-- http://chimera.labs.oreilly.com/books/1230000000929/ch07.html#sec_conc-logger
-- logger.hs

import Control.Concurrent
import Control.Monad
import System.IO

data Logger = Logger (MVar LogCommand)

data LogCommand = Message String | Stop (MVar ())

-- Message to log
-- request to the logging thread to terminate
-- MVar () waited upon by requestor of termination
-- requestor knows when logging has terminated, then

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let lgr = Logger m
  _ <- forkIO (logger lgr)
  return lgr


logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          _ <- putStrLn msg
          loop
        Stop s -> do
          _ <- putStrLn "logger is terminating"
          putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)  -- will block until MVar `m` available

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  _ <- putMVar m (Stop s)
  takeMVar s


main :: IO ()
main = do
  lgr <- initLogger
  _ <- logMessage lgr "Hello, first log message"
  _ <- logMessage lgr "Hello again, second log message"
  logStop lgr


logCount :: Logger -> Int -> Int -> IO ()
logCount lgr lower upper =
  forM_ [lower..upper] (\i -> do
                           _ <- threadDelay (10^6)
                           logMessage lgr ("count " ++ (show i))
                       )

-- this MVar is a "one place channel"

logCountsExample :: IO ()
logCountsExample = do
  lgr <- initLogger
  tid1 <- forkIO $ logCount lgr 1 10
  _ <- putStrLn $ "thread id: "++(show tid1)
  tid2 <- forkIO $ logCount lgr 20 35
  _ <- putStrLn $ "thread id: "++(show tid2)
  tid3 <- forkIO $ logCount lgr 100 130
  _ <- putStrLn $ "thread id: "++(show tid3)
  _ <- threadDelay (20 * 10^6) -- 20 seconds
  _ <- putStrLn "sending logger stop message"
  logStop lgr


-- http://chimera.labs.oreilly.com/books/1230000000929/ch07.html#sec_conc-phonebook
-- We can take any pure immutable data structure such as Map and turn it into mutable shared state by simply wrapping it in an MVar.
  
