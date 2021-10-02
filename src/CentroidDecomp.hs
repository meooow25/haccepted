{-
Centroid decomposition

A recursive divide-and-conquer decomposition of a tree into multiple subtrees.
This allows performing certain operations involving paths on the original tree effectively, by
taking every path on the tree into account exactly once when it passes through the root of a
decomposed subtree. The roots of the subtrees are chosen to be centroids so that the recursive
decomposition has logarithmic depth.

Sources:
* https://petr-mitrichev.blogspot.com/2015/03/this-week-in-competitive-programming_22.html
* https://github.com/cheran-senthil/PyRival/blob/master/pyrival/graphs/centroid_decomposition.py

centroidDecompose
Performs centroid decomposition on a tree of n nodes, returning a list of n trees. O(n log n).
-}

module CentroidDecomp
    ( centroidDecompose
    ) where

import Data.List
import Data.Tree

centroidDecompose :: Tree a -> [Tree a]
centroidDecompose t = fmap fst <$> go (withSize t) [] where
    withSize (Node u ts) = Node (u, sz) ts' where
        ts' = map withSize ts
        sz = 1 + sum (map (snd . rootLabel) ts') :: Int
    reroot (Node (u, sz) ts) = go u ts where
        go u uts = case partition ((>sz) . (*2) . snd . rootLabel) uts of
            ([], _) -> Node (u, sz) uts
            (~[Node (v, vsz) vts], uts') -> go v vts' where
                vts' = let usz' = sz - vsz in usz' `seq` Node (u, usz') uts' : vts
    go t acc = t' : foldr go acc (subForest t') where t' = reroot t
