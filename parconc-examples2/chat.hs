{-
   Adapted from haskell-chat-sever-example which is
      Copyright (c) 2012, Joseph Adams

   Modifications (c) 2012, Simon Marlow
-}

{-# LANGUAGE RecordWildCards #-}
module Main where

import ConcurrentUtils

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Map as Map
import Data.Map (Map)
import System.IO
import Control.Exception
import Network
import Control.Monad
import Text.Printf

{-
Notes

- protocol:
    Server: "Name?"
    Client: <string>
    -- if <string> is already in use, ask for another name
    -- Commands:
    --   /tell <string> message...  (single-user tell)
    --   /quit                      (exit)
    --   /kick <string>             (kick another user)
    --   message...                 (broadcast to all connected users)

- a client needs to both listen for commands from the socket and
  listen for activity from other clients.  Therefore we're going to
  need at least two threads per client (for listening to multiple
  things).  Easiest is to use STM for in-process communication, and to
  have a receiving thread that listens on the socket and forwards to a
  TChan.

- Handle all errors properly, be async-exception safe

- Consistency:
  - if two clients simultaneously kick a third client, only one will be
    successful

See doc/lab-exercises.tex for some ideas for enhancements that you
could try.

-}

-- <<main
main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
      (handle, host, port) <- accept sock
      printf "Accepted connection from %s: %s\n" host (show port)
      forkFinally (talk handle server) (\_ -> hClose handle)

port :: Int
port = 44444
-- >>


-- ---------------------------------------------------------------------------
-- Data structures and initialisation

-- <<Client
type ClientName = String

data Client = Client
  { clientName     :: ClientName
  , clientHandle   :: Handle
  , clientKicked   :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  , clientBroadcastChan :: TChan OutgoingBroadcast
  }
-- >>

-- <<newClient
newClient :: TChan OutgoingBroadcast -> ClientName -> Handle -> STM Client
newClient broad name handle = do
  c <- newTChan
  k <- newTVar Nothing
  -- broad <- newTChan -- duplicate server's broadcast channel
  broad' <- dupTChan broad
  return Client { clientName     = name
                , clientHandle   = handle
                , clientSendChan = c
                , clientKicked   = k
                , clientBroadcastChan = broad'
                }
-- >>

-- <<Server
data Server = Server
  { clients :: TVar (Map ClientName Client)
  , broadcastChannel :: TChan OutgoingBroadcast
  }

newServer :: IO Server
newServer = do
  c <- newTVarIO Map.empty
  broad <- newTChanIO
  return Server { clients = c, broadcastChannel = broad }
-- >>

-- <<Message
data Message = IndividualNotice String
             | Tell ClientName String
             | IncomingBroadcast ClientName String
             | Command String

data OutgoingBroadcast = OutgoingBroadcast ClientName String
                       | NoticeBroadcast String

instance Show OutgoingBroadcast where
  show (OutgoingBroadcast name str) = name ++ ": " ++ str
  show (NoticeBroadcast str) = "**Server** " ++ str

-- >>

-- -----------------------------------------------------------------------------
-- Basic operations

-- <<broadcast
broadcast :: Server -> OutgoingBroadcast -> STM ()
broadcast Server{..} msg =
  writeTChan broadcastChannel msg

  -- do
  -- clientmap <- readTVar clients
  -- mapM_ (\client -> sendMessage client msg) (Map.elems clientmap)
-- >>

-- <<sendMessage
sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan clientSendChan msg
-- >>

-- <<sendToName
sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName server@Server{..} name msg = do
  clientmap <- readTVar clients
  case Map.lookup name clientmap of
    Nothing     -> return False
    Just client -> sendMessage client msg >> return True
-- >>

tell :: Server -> Client -> ClientName -> String -> IO ()
tell server@Server{..} Client{..} who msg = do
  ok <- atomically $ sendToName server who (Tell clientName msg)
  if ok
     then return ()
     else hPutStrLn clientHandle (who ++ " is not connected.")

kick :: Server -> ClientName -> ClientName -> STM ()
kick server@Server{..} who by = do
  clientmap <- readTVar clients
  case Map.lookup who clientmap of
    Nothing ->
      void $ sendToName server by (IndividualNotice $ who ++ " is not connected")
    Just victim -> do
      writeTVar (clientKicked victim) $ Just ("by " ++ by)
      void $ sendToName server by (IndividualNotice $ "you kicked " ++ who)

-- -----------------------------------------------------------------------------
-- The main server

talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode
      -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  readName
 where
-- <<readName
  readName = do
    hPutStrLn handle "What is your name?"
    name <- hGetLine handle
    if null name
      then readName
      else mask $ \restore -> do        -- <1>
             ok <- checkAddClient server name handle
             case ok of
               Nothing -> restore $ do  -- <2>
                  hPrintf handle
                     "The name %s is in use, please choose another\n" name
                  readName
               Just client ->
                  restore (runClient server client) -- <3>
                      `finally` removeClient server name
-- >>

-- <<checkAddClient
checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if Map.member name clientmap
    then return Nothing
    else do client <- newClient broadcastChannel name handle
            writeTVar clients $ Map.insert name client clientmap
            broadcast server  $ NoticeBroadcast (name ++ " has connected")
            return (Just client)
-- >>

-- <<removeClient
removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' clients $ Map.delete name
  broadcast server $ NoticeBroadcast (name ++ " has disconnected")
-- >>

receiveBroadcast :: Client -> IO ()
receiveBroadcast Client{..} = do 
  _ <- putStrLn "fork `receiveBroadcast` thread:"
  forever $ do  -- need to shut down this thread when client disconnects
    incomingBroadcast <- atomically $ readTChan clientBroadcastChan
    _ <- putStrLn $ show incomingBroadcast
    hPutStrLn clientHandle $ show incomingBroadcast

-- <<runClient
runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  -- _ <- forkIO $ receiveBroadcast client
  -- _ <- putStrLn "`race server receive`"
  -- _ <- race server receive
  -- _ <- putStrLn "`race receive (receiveBroadcast client)`"
  -- _ <- race receive (receiveBroadcast client)
  _ <- putStrLn "`race (race server receive) (receiveBroadcast client)`"
  _ <- race (race server receive) (receiveBroadcast client)
  -- _ <- race server receiveBroadcast
  return ()
 where
   receive :: IO ()
   receive = forever $ do
     msg <- hGetLine clientHandle
     atomically $ sendMessage client (Command msg)

   server = join $ atomically $ do
     k <- readTVar clientKicked
     case k of
       Just reason -> return $
         hPutStrLn clientHandle $ "You have been kicked: " ++ reason
       Nothing -> do
         msg <- readTChan clientSendChan
         return $ do
           continue <- handleMessage serv client msg
           when continue $ server
-- >>

-- <<handleMessage
handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
  case message of
     IndividualNotice msg         -> output $ "*** " ++ msg
     Tell name msg      -> output $ "*" ++ name ++ "*: " ++ msg
     IncomingBroadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
     Command msg ->
       case words msg of
           ["/kick", who] -> do
               atomically $ kick server who clientName
               return True
           "/tell" : who : what -> do
               tell server client who (unwords what)
               return True
           ["/quit"] ->
               return False
           ('/':_):_ -> do
               hPutStrLn clientHandle $ "Unrecognised command: " ++ msg
               return True
           _ -> do
               atomically $ broadcast server $ OutgoingBroadcast clientName msg
               return True
 where
   output s = do hPutStrLn clientHandle s; return True
-- >>
