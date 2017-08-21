module BankAccount where

-- https://en.wikipedia.org/wiki/Concurrent_Haskell

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
import System.Exit (exitSuccess)

type Account = TVar Integer

main = do
    bob <- newAccount 10000
    jill <- newAccount 4000
    repeatIO 2000 $ forkIO $ atomically $ transfer 1 bob jill
    forever $ do
        bobBalance <- atomically $ readTVar bob
        jillBalance <- atomically $ readTVar jill
        putStrLn ("Bob's balance: " ++ show bobBalance ++ ", Jill's balance: " ++ show jillBalance)
        if bobBalance == 8000
            then exitSuccess
            else putStrLn "Trying again."

repeatIO :: Integer -> IO a -> IO a
repeatIO 1 m = m
repeatIO n m = m >> repeatIO (n - 1) m

newAccount :: Integer -> IO Account
newAccount amount = newTVarIO amount

transfer :: Integer -> Account -> Account -> STM ()
transfer amount from to = do
    fromVal <- readTVar from
    if (fromVal - amount) >= 0
        then do
               debit amount from
               credit amount to
        else retry

credit :: Integer -> Account -> STM ()
credit amount account = do
    current <- readTVar account
    writeTVar account (current + amount)

debit :: Integer -> Account -> STM ()
debit amount account = do
    current <- readTVar account
    writeTVar account (current - amount)
