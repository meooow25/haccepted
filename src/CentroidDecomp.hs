{-
Centroid decomposition

A recursive decomposition (divide-and-conquer) of a tree into multiple subtrees.
This allows performing certain operations involving paths on the original tree effectively, by
taking every path on the tree into account exactly once when it passes through the root of a
decomposed subtree. The roots of the subtrees are chosen to be centroids so that the recursive
decomposition has logarithmic depth.

Sources:
* https://petr-mitrichev.blogspot.com/2015/03/this-week-in-competitive-programming_22.html
* https://github.com/cheran-senthil/PyRival/blob/master/pyrival/graphs/centroid_decomposition.py

Implementation notes:
This is one of those things likely to TLE :(
A typical imperative style implementation would modify the graph in place between calculations,
taking O(n log n) time but no extra memory. In functional style, all the trees get constructed so it
needs O(n log n) time _and_ memory.
Some possible optimizations which may or may not make things fast enough:
* The decomposition is done on a tree of (node, tree size), and the tree size is dropped off before
  returning the result. This is quite wasteful, and this step can be skipped if the caller works on
  trees of (node, tree size), possibly ignoring the tree size.
* Sometimes the centroid tree structure is not required, just the trees themselves. It is a little
  faster to modify the function to return a list of trees than calling toList on the returned tree.

centroidDecompose
Performs centroid decomposition on a tree of n nodes, returning the decomposition as a tree of
n trees. O(n log n).
-}

module CentroidDecomp
    ( centroidDecompose
    ) where

import Data.List
import Data.Tree

centroidDecompose :: Tree a -> Tree (Tree a)
centroidDecompose t = fmap fst <$> go (withSize t) where
    withSize (Node u ts) = Node (u, sz) ts' where
        ts' = map withSize ts
        sz = 1 + sum (map (snd . rootLabel) ts') :: Int
    reroot (Node (u, sz) ts) = go u ts where
        go u uts = case partition ((>sz) . (*2) . snd . rootLabel) uts of
            ([], _) -> Node (u, sz) uts
            (~[Node (v, vsz) vts], uts') -> go v vts' where
                vts' = let usz' = sz - vsz in usz' `seq` Node (u, usz') uts' : vts
    go t = Node t' $ map go $ subForest t' where t' = reroot t
