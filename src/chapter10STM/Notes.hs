module Chapter10STM.Notes where

-- data STM a -- abstract
-- instance Monad STM -- available

-- atomically :: STM a -> IO a

-- data TVar a -- abstract
-- newTVar :: a -> STM (TVar a)
-- readTVar :: TVar a -> STM a
-- writeTVar :: TVar a -> a -> STM ()

-- retry :: STM a
-- orElse :: STM a -> STM a -> STM a

-- throwSTM :: Exception e => e -> STM a
-- catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a
