module Node where

import Data.IORef
import Control.Applicative
import Event

data Status = Active | Inactive

data Node a = Node
  { status :: IORef Status
  , state  :: IORef a
  , event  :: Event a
  }

newNode :: Status -> a -> IO (Node a)
newNode status a = Node <$> newIORef (status, a) <*> newEvent

fireNode :: Node a -> IO ()
fireNode node = do
    a <- readIORef (state node)
    fire (event node) a

connectNode :: Node a -> Node b -> (a -> b -> b) -> IO ()
connectNode from to f = listen (event from) $ \a -> do
                          modifyIORef (state to) (f a)
