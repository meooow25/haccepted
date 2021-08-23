{-
Lowest common ancestor queries on a forest

Uses an Euler tour and a sparse table for range minimum queries

Sources:
* https://en.wikipedia.org/wiki/Lowest_common_ancestor
* Michael Bender and Martin Farach-Colton, "The LCA Problem Revisited", 2000
  https://www.ics.uci.edu/~eppstein/261/BenFar-LCA-00.pdf

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
import Data.Array
import Data.Foldable
import Data.Graph
import Data.Semigroup
import Data.Tuple

import SparseTable ( querySP, fromListSP, SparseTable )

data LCA = LCA !(SparseTable (Min Int)) !(Array Int Int) !(Array Int Int) deriving Show

buildLCA :: Bounds -> [Tree Vertex] -> LCA
buildLCA (l, r) _ | l > r = error "empty range"
buildLCA (l, r) ts = LCA sp itime first where
    n = r - l + 2
    rt = Node (l - 1) ts
    itime = listArray (1, n) $ toList rt
    time = array (l - 1, r) $ map swap $ assocs itime
    euler = go rt [] where
        go (Node u ts) acc = foldr (\node acc -> x : go node acc) (x:acc) ts where x = (time!u, u)
    first = accumArray min (2 * n) (l - 1, r) $ zip (map snd euler) [1..]
    sp = fromListSP (1, 2 * n - 1) $ map (Min . fst) euler

queryLCA' :: a -> (Vertex -> a) -> Vertex -> Vertex -> LCA -> a
queryLCA' def tf u v (LCA sp itime first) = y where
    (fu, fv) = (first!u, first!v)
    (fu', fv') = (min fu fv, max fu fv)
    x = itime ! getMin (querySP fu' fv' sp)
    y = if x == fst (bounds first) then def else tf x

queryLCA :: Vertex -> Vertex -> LCA -> Vertex
queryLCA = queryLCA' (error "no LCA") id

query1LCA :: Vertex -> Vertex -> LCA -> Maybe Vertex
query1LCA = queryLCA' Nothing Just

--------------------------------------------------------------------------------
-- For tests

instance NFData LCA where
    rnf (LCA sp itime first) = rnf sp `seq` rnf itime `seq` rnf first
