{-# LANGUAGE RecordWildCards #-}
module Control.Concurrent.Thread.BoundedThreadGroup where
import Control.Concurrent.Thread.Group (ThreadGroup)
import qualified Control.Concurrent.Thread as Thread
import qualified Control.Concurrent.Thread.Group as ThreadGroup
import Control.Concurrent 

new :: Int -> IO BoundedThreadGroup
new maxSize = do 
  threadGroup <- ThreadGroup.new 
  return BoundedThreadGroup {..}

wait :: BoundedThreadGroup -> IO ()
wait BoundedThreadGroup {..} = ThreadGroup.wait threadGroup

data BoundedThreadGroup = BoundedThreadGroup 
    { maxSize     :: Int
    , threadGroup :: ThreadGroup
    }
    
forkIO :: BoundedThreadGroup -> IO a -> IO (ThreadId, IO (Thread.Result a))
forkIO BoundedThreadGroup {..} action = do
  ThreadGroup.waitN maxSize threadGroup
  ThreadGroup.forkIO threadGroup action 

