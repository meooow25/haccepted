{-
Lowest common ancestor queries on a forest

Uses an Euler tour and a sparse table for range minimum queries, with an range size optimization
from 2n to n, adapted from PyRival.

Sources:
* https://en.wikipedia.org/wiki/Lowest_common_ancestor
* Michael Bender and Martin Farach-Colton, "The LCA Problem Revisited", 2000
  https://www.ics.uci.edu/~eppstein/261/BenFar-LCA-00.pdf
* https://github.com/cheran-senthil/PyRival/blob/master/pyrival/graphs/lca.py

buildLCA
Build a structure for LCA queries on a tree. O(n log n).

queryLCA
Query the LCA of two nodes in a tree. O(1).

build1LCA
Build a structure for LCA queries on a forest. O(n log n).

query1LCA
Query the LCA of two nodes in a forest. O(1).

Implementation note:
l - 1 is taken as a dummy root for a forest, converting it to a tree to make things simpler.
-}
module LCA
    ( LCA
    , buildLCA
    , queryLCA
    , build1LCA
    , query1LCA
    ) where

import Control.DeepSeq
import Data.Array.Unboxed
import Data.Foldable
import Data.Graph
import Data.Semigroup

import SparseTable ( fromListSP, query1SP, SparseTable )

data LCA = LCA !(SparseTable (Min Int)) !(UArray Int Int) !(UArray Int Int) deriving Show

buildLCA :: Bounds -> Tree Vertex -> LCA
buildLCA (l, r) _ | l > r = error "empty range"
buildLCA (l, r) t = LCA sp time itime where
    n = r - l + 1
    itime = listArray (1, n) $ toList t
    time = array (l, r) $ zip (toList t) [1..]
    euler = go t [] where
        go (Node u ts) acc = foldr (\node acc -> time!u : go node acc) acc ts
    sp = fromListSP (1, n - 1) $ map Min euler

queryLCA :: Vertex -> Vertex -> LCA -> Vertex
queryLCA u v (LCA sp time itime) = x where
    (fu, fv) = (time!u, time!v)
    (l, r) = bounds time
    x | u /= v         = itime ! getMin (query1SP (min fu fv) (max fu fv - 1) sp)
      | u < l || r < u = error "invalid node"
      | otherwise      = u

build1LCA :: Bounds -> [Tree Vertex] -> LCA
build1LCA (l, r) ts = buildLCA (l - 1, r) $ Node (l - 1) ts

query1LCA :: Vertex -> Vertex -> LCA -> Maybe Vertex
query1LCA u v lca@(LCA _ time _) = if x == l then Nothing else Just x where
    l = fst $ bounds time
    x = queryLCA u v lca

--------------------------------------------------------------------------------
-- For tests

instance NFData LCA where
    rnf (LCA sp _ _) = rnf sp
