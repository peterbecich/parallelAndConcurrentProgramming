{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Database (
       Database,
       Key, Value,
       createDB,
       get, set,
       rcdata,
  ) where


import GHC.Generics (Generic)
import Data.Typeable
import Data.Binary hiding (get)

import Control.Distributed.Process hiding (Message)

import qualified Data.Map as Map
import Data.Map (Map, insert, empty)

import Control.Monad

type Key   = String
type Value = String

-- type Database = ProcessId
data Database = Database
  { storage :: Map Key Value
  , spid :: ProcessId
  }

-- replace with Lens
insertkv :: Database -> Key -> Value -> Database
insertkv db k v = let
  sp = spid db
  st = storage db
  st' = insert k v st
  in Database st' sp

data Request = Set Key Value
             | Get Key
  deriving (Typeable, Generic)

type Response = Maybe Value

instance Binary Request
-- instance Binary Response

-- needless IO?
-- handleRequest :: Database -> Request -> IO (Database, Response)
-- handleRequest db (Set k v) = do
--    db' <- pure $ insertkv db k v
--    let response = Just v
--    pure (db', response)

handleRequest :: Database -> Request -> (Database, Response)
handleRequest db (Set k v) = let
  db' = insertkv db k v
  response = Just v
  in (db', response)
handleRequest db (Get k) = (db, Map.lookup k (storage db))

createDB :: [NodeId] -> Process Database
createDB nodes = do
  pid <- getSelfPid
  -- put storage here?  KV Map?
  pure $ Database empty pid
  
set :: Database -> Key -> Value -> Process ()
set db k v = do
  send (spid db) $ Set k v

get :: Database -> Key -> Process Response
get db k = do
  send (spid db) $ Get k
  expect

rcdata :: RemoteTable -> RemoteTable
rcdata = id
  -- For the exercise, change this to include your
  -- remote metadata, e.g. rcdata = Database.__remoteTable
