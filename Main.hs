import Node
import Graph
import Control.Monad

graph :: GraphM ()
graph = do
    i1 <- input
    i2 <- input
    n  <- node 100
    o  <- output
    connect i1 n 50
    connect i2 n 50
    connect n  o 100

main :: IO ()
main = do
    g <- build graph
    _ <- step g [100, 100]
    step g [100, 100] >>= print
