module Node
  ( Node(..)
  , Edge(..)
  , Graph(..)
  , GraphM
  , node
  , edge
  ) where

import Data.Monoid
import Data.Unique
import qualified Data.IntMap as IM
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.IO.Class

type Number = Int
type Ident  = Int

data Status = Active | Inactive

data Node = Node
  { nodeId    :: Ident
  , shreshold :: Number
  }

data Edge = Edge
  { edgeId :: Ident
  , nodef  :: Ident
  , nodet  :: Ident
  , ratio  :: Number
  }

data Graph = Graph
  { nodes :: IM.IntMap Node
  , edges :: IM.IntMap Edge
  }

emptyGraph = Graph IM.empty IM.empty

type GraphM = StateT Graph IO

node :: Status -> Number -> GraphM Ident
node st a = do
    nid <- hashUnique <$> liftIO newUnique
    let n = Node nid st a
    modify $ \g -> g{nodes=IM.insert nid n (nodes g)}
    return nid

edge :: Ident -> Ident -> Number -> GraphM Ident
edge nodef nodet ratio = do
    eid <- hashUnique <$> liftIO newUnique
    let e = Edge eid nodef nodet ratio
    modify $ \g -> g{edges=IM.insert eid e (edges g)}
    return eid

build :: GraphM () -> IO Graph
build m = execStateT m emptyGraph
