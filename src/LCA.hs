{-|
Lowest common ancestor queries on a tree

Uses an Euler tour and a sparse table for range minimum queries, with an range size optimization
from 2n to n, adapted from PyRival.

Sources:
* https://en.wikipedia.org/wiki/Lowest_common_ancestor
* Michael Bender and Martin Farach-Colton, "The LCA Problem Revisited", 2000
  https://www.ics.uci.edu/~eppstein/261/BenFar-LCA-00.pdf
* https://github.com/cheran-senthil/PyRival/blob/master/pyrival/graphs/lca.py

Implementation notes:
* l - 1 is taken as a dummy root for a forest, converting it to a tree to make things simpler.

buildLCA
Build a structure for LCA queries on a tree. O(n log n).

queryLCA
Query the LCA of two nodes in a tree. O(1).

build1LCA
Build a structure for LCA queries on a forest. O(n log n).

query1LCA
Query the LCA of two nodes in a forest. O(1).
-}

module LCA
    ( LCA
    , buildLCA
    , queryLCA
    , build1LCA
    , query1LCA
    ) where

import Control.DeepSeq
import Data.Array.ST
import Data.Array.Unboxed
import Data.Foldable
import Data.Graph

import SparseTable ( buildSP, foldISP )

data LCA = LCA !(UArray (Int, Int) Int) !(UArray Vertex Int) !(UArray Int Vertex) deriving Show

buildLCA :: Bounds -> Tree Vertex -> LCA
buildLCA (l, r) _ | l > r = error "buildLCA: empty range"
buildLCA (l, r) t = LCA sp time itime where
    n = r - l + 1
    itime = listArray (1, n) $ toList t
    time = array (l, r) [(x, i) | (i, x) <- assocs itime]
    euler = go t [] where
        go (Node u ts) = let x = time!u in x `seq` foldr ((.) . ((x:) .) . go) id ts
    sp = if n > 1 then runSTUArray $ buildSP min (1, n-1) euler else listArray ((1,1),(0,0)) []

queryLCA :: Vertex -> Vertex -> LCA -> Vertex
queryLCA u v (LCA sp time itime)
    | u < l || r < u = error "queryLCA: invalid node"
    | u == v         = u
    | otherwise      = itime ! foldISP min sp (min fu fv) (max fu fv - 1)
  where
    (l, r) = bounds time
    (fu, fv) = (time!u, time!v)

build1LCA :: Bounds -> [Tree Vertex] -> LCA
build1LCA (l, r) = buildLCA (l - 1, r) . Node (l - 1)

query1LCA :: Vertex -> Vertex -> LCA -> Maybe Vertex
query1LCA u v lca@(LCA _ time _) = if x == l then Nothing else Just x where
    (l, _) = bounds time
    x = queryLCA u v lca

--------------------------------------------------------------------------------
-- For tests

instance NFData LCA where
    rnf = rwhnf
