module Graph
  ( Graph(..)
  , GraphM
  , emptyGraph
  , node
  , input
  , output
  , connect
  , build
  , step
  ) where

import qualified Data.IntMap as IM
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Applicative
import Node

data Graph = Graph
  { inputs :: [Node]
  , outputs :: [Node]
  , nodes :: [Node]
  , edges :: IM.IntMap [NodeInput]
  }

emptyGraph :: Graph
emptyGraph = Graph [] [] [] IM.empty

type GraphM = StateT Graph IO

modEdges f g = g { edges = f (edges g) }
modNodes f g = g { nodes = f (nodes g) }
modInputs f g = g { inputs = f (inputs g) }
modOutputs f g = g { outputs = f (outputs g) }

node :: Int -> GraphM Node
node shres = do
    n <- liftIO $ newNode shres
    modify $ modNodes (n:)
    return n

input :: GraphM Node
input = do
    n <- liftIO newSimpleNode
    modify $ modNodes (n:)
    modify $ modInputs (n:)
    return n

output :: GraphM Node
output = do
    n <- liftIO newSimpleNode
    --n <- liftIO (newNode 50)
    modify $ modNodes (n:)
    modify $ modOutputs (n:)
    return n

connect :: Node -> Node -> Int -> GraphM ()
connect nodef nodet ratio = do
    let input = NodeInput nodef ratio
    modify $ modEdges $
      IM.alter
        (maybe (Just [input]) (Just . (input:)))
        (nid nodet)

build :: GraphM () -> IO Graph
build m = execStateT m emptyGraph

feedNode :: Node -> [NodeInput] -> IO ()
feedNode node inputs = do
    input <- sum <$> mapM get1 inputs
    feed node input
  where
    get1 (NodeInput n r) = do
        o <- getOutput n
        return $ (o * r) `div` 100

step :: Graph -> [Int] -> IO [Int]
step g vs = do
    mapM_ (uncurry feed) $ zip (inputs g) vs
    mapM_ feedinput (nodes g)
    mapM_ update (nodes g)
    mapM  getOutput (outputs g)
  where
    feedinput n =
      case IM.lookup (nid n) (edges g) of
        Just inputs -> feedNode n inputs
        Nothing     -> return ()
