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

import Data.Tree

import LabelledGraph ( LTree(..) )
import Misc ( farthest )

centroidDecompose :: Tree a -> Tree (Tree a)
centroidDecompose t = go t (szTree t) where
    szTree (Node _ ts) = sz `seq` Node sz szts where
        szts = map szTree ts
        sz   = 1 + sum (map rootLabel szts) :: Int
    go (Node r rts) (Node sz rszts) = case farthest step (r, rts, rszts) of
        (u, uts, uszts) -> Node (Node u uts) (zipWith go uts uszts)
      where
        step (u, uts, uszts) = mkv <$> removeOne ((>sz) . (*2) . rootLabel) uts uszts where
            mkv (Node v vts, Node vsz vszts, uts', uszts') = (v, vts', vszts') where
                vts'   = Node u uts' : vts
                vszts' = let usz' = sz - vsz in usz' `seq` Node usz' uszts' : vszts

centroidDecomposeL :: LTree b a -> Tree (LTree b a)
centroidDecomposeL t = go t (szTree t) where
    szTree (LNode _ ts) = sz `seq` Node sz szts where
        szts = map (szTree . snd) ts
        sz   = 1 + sum (map rootLabel szts) :: Int
    go (LNode r rts) (Node sz rszts) = case farthest step (r, rts, rszts) of
        (u, uts, uszts) -> Node (LNode u uts) (zipWith go (snd <$> uts) uszts)
      where
        step (u, uts, uszts) = mkv <$> removeOne ((>sz) . (*2) . rootLabel) uts uszts where
            mkv ((l, LNode v vts), Node vsz vszts, uts', uszts') = (v, vts', vszts') where
                vts'   = (l, LNode u uts') : vts
                vszts' = let usz' = sz - vsz in usz' `seq` Node usz' uszts' : vszts

removeOne :: (b -> Bool) -> [a] -> [b] -> Maybe (a, b, [a], [b])
removeOne p = go where
    go [] [] = Nothing
    go (a:as) (b:bs)
        | p b       = Just (a, b, as, bs)
        | otherwise = (\(a', b', as', bs') -> (a', b', a:as', b:bs')) <$> go as bs
    go _ _ = error "bad input"
