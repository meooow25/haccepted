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
Build a structure for LCA queries. O(n log n).

queryLCA
Query the LCA of two nodes. O(1).

query1LCA
Query the LCA of two nodes that may or may not be connected. O(1).

Implementation note:
l - 1 is taken as a dummy root, converting the forest to a tree to make things simpler.
-}
module LCA
    ( buildLCA
    , queryLCA
    , query1LCA
    ) where

import Control.DeepSeq
import Data.Array.Unboxed
import Data.Foldable
import Data.Graph
import Data.Semigroup

import SparseTable ( fromListSP, query1SP, SparseTable )

data LCA = LCA !(SparseTable (Min Int)) !(UArray Int Int) !(UArray Int Int) deriving Show

buildLCA :: Bounds -> [Tree Vertex] -> LCA
buildLCA (l, r) _ | l > r = error "empty range"
buildLCA (l, r) ts = LCA sp time itime where
    n = r - l + 2
    rt = Node (l - 1) ts
    itime = listArray (1, n) $ toList rt
    time = array (l - 1, r) $ zip (toList rt) [1..]
    euler = go rt [] where
        go (Node u ts) acc = foldr (((time!u :) .) . go) acc ts
    sp = fromListSP (1, n - 1) $ map Min euler

queryLCA' :: a -> (Vertex -> a) -> Vertex -> Vertex -> LCA -> a
queryLCA' def tf u v (LCA sp time itime) = y where
    (fu, fv) = (time!u, time!v)
    (fu', fv') = (min fu fv, max fu fv - 1)
    (l, r) = bounds time
    x | u /= v         = itime ! getMin (query1SP fu' fv' sp)
      | u < l || r < u = error "invalid node"
      | otherwise      = u
    y = if x == l then def else tf x

queryLCA :: Vertex -> Vertex -> LCA -> Vertex
queryLCA = queryLCA' (error "no LCA") id

query1LCA :: Vertex -> Vertex -> LCA -> Maybe Vertex
query1LCA = queryLCA' Nothing Just

--------------------------------------------------------------------------------
-- For tests

instance NFData LCA where
    rnf (LCA sp _ _) = rnf sp
