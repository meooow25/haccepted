{-# LANGUAGE FlexibleContexts #-}
{-
Disjoint set union

Data structure to maintain disjoint sets of Ints, supporting find and union.
Uses union by size and path compression.

Sources:
* https://en.wikipedia.org/wiki/Disjoint-set_data_structure
* https://cp-algorithms.com/data_structures/disjoint_set_union.html
* https://github.com/kth-competitive-programming/kactl/blob/main/content/data-structures/UnionFind.h

Implementation notes:
* KACTL's optimization is used where a single array is used for both size and parent, the size
  stored as negative.
* There is no way to make this structure functional without making the complexity worse :(

Use unboxed arrays (IOUArray/STUArray) for best performance!
Let n = r - l + 1 in all instances below.
α(n) is the inverse Ackermann function.

newD
Creates a new DSU structure with elements in the range (l, r), each in its own set. O(n).

findD
Finds the representative of the set containing some element. Amortized O(α(n)).

unionD
Unites the sets containing the two elements. If they are already in the same set, returns False,
otherwise performs the union and returns True. Amortized O(α(n)).
-}

module DSU where

import Control.Monad
import Data.Array.MArray

newD :: MArray a Int m => (Int, Int) -> m (a Int Int)
newD (l, _) | l < 0 = error "negative range"
newD bnds = newArray bnds (-1)

findD :: MArray a Int m => a Int Int -> Int -> m Int
findD d = go where
    go i = do
        p <- readArray d i
        if p < 0 then pure i else do
            r <- go p
            r <$ writeArray d i r

unionD :: MArray a Int m => a Int Int -> Int -> Int -> m Bool
unionD d i j = join (go <$> findD d i <*> findD d j) where
    go i j
        | i == j    = pure False
        | otherwise = True <$ join (upd i j <$> readArray d i <*> readArray d j)
    upd i j si sj
        | si > sj   = upd j i sj si
        | otherwise = writeArray d i (si + sj) >> writeArray d j i
