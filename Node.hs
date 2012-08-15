module Node where

import Data.IORef
import Control.Applicative
import Event

data Status = Active | Inactive

data Node a = Node
  { status    :: IORef Status
  , state     :: IORef a
  , event     :: Event a
  , threshold :: a
  }

newNode :: Status -> a -> a -> IO (Node a)
newNode status a s = Node <$> newIORef status <*> newIORef a <*> newEvent <*> pure s

fireNode :: Node a -> IO ()
fireNode node = do
    a <- readIORef (state node)
    fire (event node) a

connectNode :: Ord b => Node a -> Node b -> (a -> b -> b) -> IO ()
connectNode from to f = listen (event from) $ \a -> do
                          modifyIORef (state to) (f a)
                          b <- readIORef (state to)
                          writeIORef (status to) (if (b>=threshold to) then Active else Inactive)
