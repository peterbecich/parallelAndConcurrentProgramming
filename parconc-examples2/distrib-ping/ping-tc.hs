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
data Message = Ping (SendPort ProcessId)
  deriving (Typeable, Generic)

instance Binary Message
-- >>

-- <<pingServer
pingServer :: Process ()
pingServer = do
  _ <- liftIO $ putStrLn "top of `pingServer` do-block"
  Ping chan <- expect
  _ <- liftIO $ putStrLn $ "Ping containing channel received " ++ (show chan)
  say $ printf "ping received from %s" (show chan)
  mypid <- getSelfPid
  _ <- liftIO $ putStrLn $ "my PID: " ++ (show mypid)
  sendChan chan mypid
  -- Ping second <- receiveChan chan
  -- _ <- liftIO $ putStrLn $ "second Ping received, this time by channel"
  -- sendChan chan 
  
-- >>

-- <<remotable
remotable ['pingServer]
-- >>

-- <<master
master :: [NodeId] -> Process ()
master peers = do
  _ <- liftIO $ putStrLn "top of `master` do-block"
  _ <- liftIO $ putStrLn "peers:"
  _ <- liftIO $ forM_ peers (\peer -> putStrLn $ show peer)

  _ <- liftIO $ putStrLn "first ping to each slave"
  ps <- forM peers $ \nid -> do
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'pingServer)

  _ <- liftIO $ putStrLn "mapM_ monitor ps"
  mapM_ monitor ps

  _ <- liftIO $ putStrLn "ports"
  ports <- forM ps $ \pid -> do
    say $ printf "pinging %s" (show pid)
    (sendport,recvport) <- newChan      -- <1>
    send pid (Ping sendport)            -- <2>
    return recvport

  forM_ ports $ \port -> do             -- <3>
     _ <- receiveChan port
     return ()

  say "All pongs successfully received"
  terminate
-- >>

-- <<main
main :: IO ()
main = distribMain master Main.__remoteTable
-- >>
