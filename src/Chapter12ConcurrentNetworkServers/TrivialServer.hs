module Chapter12ConcurrentNetworkServers.TrivialServer where

-- server.hs

import ConcurrentUtils
import Network
import Control.Monad
import Control.Concurrent (forkIO, myThreadId)
import System.IO
import Text.Printf
import Control.Exception

-- this server does not properly free the port 44444 after being interrupted
-- with Control-C
-- <<main
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))              -- <1>
  printf "Listening on port %d\n" port
  forever $ do                                                   -- <2>
     (handle, host, port) <- accept sock                         -- <3>
     printf "Accepted connection from %s: %s\n" host (show port)
     forkFinally (talk handle) (\_ -> hClose handle)             -- <4>

port :: Int
port = 44444
-- >>


--  main 
-- Listening on port 44444
-- Accepted connection from localhost: 45826
-- Server thread ID: ThreadId 511
-- Accepted connection from localhost: 45828
-- Server thread ID: ThreadId 512


-- <<talk
talk :: Handle -> IO ()
talk h = do
  _ <- hSetBuffering h LineBuffering
  tid <- myThreadId
  _ <- putStrLn $ "Server thread ID: "++(show tid)
  -- ^^ understand that even putStrLn utilizes a handle -- the STDOUT handle
  -- printing to console from multiple threads involves concurrency,
  -- and a shared handle
  loop                                                         -- <2>
 where
  loop = do
    line <- hGetLine h                                         -- <3>
    if line == "end"                                           -- <4>
      then hPutStrLn h ("Thank you for using the " ++         -- <5>
                         "Haskell doubling service.")
      else do
      _ <- hPutStrLn h (show (2 * (read line :: Integer))) -- <6>
      loop                                            -- <7>
-- >>
