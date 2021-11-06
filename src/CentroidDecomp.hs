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
Potential TLE warning!
All the trees get constructed so it needs O(n log n) time and memory, unlike a typical imperative
implementation which modifies the graph in place and doesn't need extra memory.
The decomposition is done on a tree of (node, tree size), and the tree size is dropped off before
returning the result. This is wasteful, so a possible optimization is to skip this step if the
caller works on trees of (node, tree size), possibly ignoring the tree size.

centroidDecompose
Performs centroid decomposition on a tree of n nodes, returning the decomposition as a tree of
n trees. O(n log n).

centroidDecomposeL
Almost identical to centroidDecompose, for edge-labelled graphs. O(n log n).
-}

module CentroidDecomp
    ( centroidDecompose
    , centroidDecomposeL
    ) where

import Data.List
import Data.Tree

import LabelledGraph ( LTree(..) )

centroidDecompose :: Tree a -> Tree (Tree a)
centroidDecompose t = fmap fst <$> go (withSize t) where
    withSize (Node u ts) = Node (u, sz) ts' where
        ts' = map withSize ts
        sz = (1 :: Int) + sum (map (snd . rootLabel) ts')
    reroot (Node (u, sz) ts) = go u ts where
        go u uts = case partition ((>sz) . (*2) . snd . rootLabel) uts of
            ([], _) -> Node (u, sz) uts
            (~[Node (v, vsz) vts], uts') -> go v vts' where
                vts' = let usz' = sz - vsz in usz' `seq` Node (u, usz') uts' : vts
    go t = Node t' $ map go $ subForest t' where t' = reroot t

centroidDecomposeL :: LTree b a -> Tree (LTree b a)
centroidDecomposeL t = fmap fst <$> go (withSize t) where
    withSize (LNode u ts) = LNode (u, sz) $ zip (map fst ts) ts' where
        ts' = map (withSize . snd) ts
        sz = (1 :: Int) + sum (map (snd . rootLabelL) ts')
    reroot (LNode (u, sz) ts) = go u ts where
        go u uts = case partition ((>sz) . (*2) . snd . rootLabelL . snd) uts of
            ([], _) -> LNode (u, sz) uts
            (~[(l, LNode (v, vsz) vts)], uts') -> go v vts' where
                vts' = let usz' = sz - vsz in usz' `seq` (l, LNode (u, usz') uts') : vts
    go t = Node t' $ map (go . snd) $ subForestL t' where t' = reroot t
