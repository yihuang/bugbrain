module Node
  ( Node(..)
  , NodeInput(..)
  , newNode
  , newSimpleNode
  ) where

import Data.IORef
import Data.Unique
import Control.Applicative

data Status = Active | Inactive
    deriving (Eq)

data Node = Node
  { nid       :: Int
  , feed      :: Int -> IO ()
  , update    :: IO ()
  , getOutput :: IO Int
  }

data NodeInput = NodeInput
  { inputNode :: Node
  , ratio     :: Int
  }

newNode :: Int -> IO Node
newNode shreshold = do
    nid <- hashUnique <$> newUnique
    st  <- newIORef Inactive
    val <- newIORef 0
    let feed = writeIORef val
        val2st v = if v>=shreshold
                     then Active
                     else Inactive
        st2val st' = if st'==Active
                       then 100
                       else 0
        update = readIORef val >>= writeIORef st . val2st
        output = st2val <$> readIORef st
    return $ Node nid feed update output

newSimpleNode :: IO Node
newSimpleNode = do
    nid <- hashUnique <$> newUnique
    val <- newIORef 0
    return $ Node
               nid
               (writeIORef val)
               (return ())
               (readIORef val)
