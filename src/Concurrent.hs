module Concurrent(
    TMessages
  , newTMessagesIO
  , popIO
  , appendIO
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (when)

-- Type with FIFO STM operations, pop and append.
type TMessages a = TVar [a]

newTMessagesIO :: IO (TVar [a])
newTMessagesIO = newTVarIO []

pop :: TMessages a -> STM a
pop list = do
  list_ <- readTVar list
  when (null list_) retry
  modifyTVar list tail
  return $ head list_

popIO :: TMessages a -> IO a
popIO list = atomically $ pop list

append :: TMessages a -> a -> STM ()
append list value = modifyTVar list (<>[value])

appendIO :: TMessages a -> a -> IO ()
appendIO list value = atomically $ append list value
