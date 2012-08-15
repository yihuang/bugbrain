{-# LANGUAGE ScopedTypeVariables #-}
module Event
  ( Event(listen, fire)
  , newEvent
  )
 where

import qualified Data.IntMap as M
import Data.IORef
import Data.Unique

{--
 - listen :: Event a -> (a -> IO ()) -> IO ()
 - fire   :: Event a -> a -> IO ()
 -}

data Event a = Event { listen :: (a -> IO ()) -> IO ()
                     , fire   :: a -> IO () 
                     }

newEvent :: IO (Event a)
newEvent = do
    handlers <- newIORef M.empty
    let listen' fn = do
            key <- fmap hashUnique newUnique
            modifyIORef handlers $ M.insert key fn
            --return (modifyIORef handlers $ M.delete key)
        fire'   a  =
            readIORef handlers >>= mapM_ ($ a) . M.elems
    return $ Event listen' fire'
