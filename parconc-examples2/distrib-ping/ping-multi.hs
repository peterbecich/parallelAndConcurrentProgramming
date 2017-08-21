{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure

import Control.Monad
import Text.Printf
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

import DistribUtils

-- <<Message
data Message = Ping ProcessId
             | Pong ProcessId
  deriving (Typeable, Generic)          -- <1>

instance Binary Message                 -- <2>
-- >>

-- <<pingServer -- slave
pingServer :: Process ()
pingServer = do
  _ <- liftIO $ putStrLn "top of `pingServer` do-block"
  Ping from <- expect                              -- <1>
  -- ^^^ will cause runtime exception if `expect` returns `Pong`
  _ <- liftIO $ putStrLn $ "ping received from " ++ (show from)
  _ <- say $ printf "ping received from %s" (show from) -- <2>
  mypid <- getSelfPid                              -- <3>
  _ <- liftIO $ putStrLn $ "my PID: " ++ (show mypid)
  send from (Pong mypid)                           -- <4>
-- >>

-- <<remotable
remotable ['pingServer]
-- >>

-- <<master
master :: [NodeId] -> Process ()                     -- <1>
master peers = do
  _ <- liftIO $ putStrLn "top of `master` do-block"
  _ <- liftIO $ putStrLn "peers:"
  _ <- liftIO $ forM_ peers (\peer -> putStrLn $ show peer)

  _ <- liftIO $ putStrLn "first ping to each slave"
  ps <- forM peers $ \nid -> do                      -- <2>
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)

  _ <- liftIO $ putStrLn "second ping to each slave"
  ps <- forM peers $ \nid -> do                      -- <2>
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)

  _ <- liftIO $ putStrLn "third ping to each slave"
  ps <- forM peers $ \nid -> do                      -- <2>
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)


  mypid <- getSelfPid

  _ <- liftIO $ putStrLn $ "my PID: " ++ (show mypid)

  forM_ ps $ \pid -> do                              -- <3>
    say $ printf "pinging %s" (show pid)
    send pid (Ping mypid)

  waitForPongs ps                                    -- <4>

  say "All pongs successfully received"
  terminate

waitForPongs :: [ProcessId] -> Process ()            -- <5>
waitForPongs [] = return ()
waitForPongs ps = do
  _ <- liftIO $ putStrLn "top of `waitForPongs` do-block"
  m <- expect
  case m of
    Pong p -> waitForPongs (filter (/= p) ps)
    _  -> say "MASTER received ping" >> terminate
-- >>

-- <<main
main :: IO ()
main = distribMain master Main.__remoteTable
-- >>
