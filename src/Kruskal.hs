{-|
Kruskal's algorithm

An algorithm to find the minimum spanning forest of an edge-weighted graph.

Sources
* https://en.wikipedia.org/wiki/Kruskal%27s_algorithm
* Joseph B. Kruskal, "On the shortest spanning subtree of a graph and the traveling salesman
  problem", 1956
  https://www.ams.org/journals/proc/1956-007-01/S0002-9939-1956-0078686-7/

kruskal
Runs Kruskal's algorithm on the graph represented by the given list of edges. Returns the edges that
are part of a minimum spanning forest. Vertices should be non-negative. O(|V| + |E|log|E|).
-}

module Kruskal where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Graph
import Data.Ord

import DSU ( newD, unionD )
import Sort ( sortBy )

type Weight = Int
data WEdge = WEdge { getU :: !Vertex, getV :: !Vertex, getW :: !Weight } deriving (Eq, Show)

kruskal :: Bounds -> [WEdge] -> [WEdge]
kruskal bnds es = runST $ do
    dsu <- newD bnds :: ST s (STUArray s Int Int)
    filterM (unionD dsu <$> getU <*> getV) $ sortBy (comparing getW) es

--------------------------------------------------------------------------------
-- For tests

instance NFData WEdge where
    rnf = rwhnf
