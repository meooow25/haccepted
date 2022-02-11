{-# LANGUAGE FlexibleContexts #-}
{-
Disjoint set union

Data structure to maintain disjoint sets of Ints, supporting find and union.
Uses union by size and path halving.

Sources:
* https://en.wikipedia.org/wiki/Disjoint-set_data_structure
* https://cp-algorithms.com/data_structures/disjoint_set_union.html
* https://github.com/kth-competitive-programming/kactl/blob/main/content/data-structures/UnionFind.h
* Robert E. Tarjan and Jan van Leeuwen, "Worst-Case Analysis of Set Union Algorithms", 1984
  https://dl.acm.org/doi/10.1145/62.2160

Implementation notes:
* KACTL's optimization is used where a single array is used for both size and parent, the size
  stored as negative.
* There is no way to make this structure functional without making the complexity worse :(

Use unboxed arrays (IOUArray/STUArray) for best performance!
n = r - l + 1 in all instances below.
α is the inverse Ackermann function.

newD
Creates a new DSU structure with elements in the range (l, r), each in its own set. O(n).

sameSetD
Returns whether the two elements are in the same set. Amortized O(α(n)).

unionD
Unites the sets containing the two elements. If they are already in the same set, returns False,
otherwise performs the union and returns True. Amortized O(α(n)).
-}

module DSU
    ( newD
    , sameSetD
    , unionD
    ) where

import Control.Monad
import Data.Array.MArray

newD :: MArray a Int m => (Int, Int) -> m (a Int Int)
newD (l, _) | l < 0 = error "negative range"
newD bnds = newArray bnds (-1)

findD :: MArray a Int m => a Int Int -> Int -> m Int
findD d = go where
    go i = readArray d i >>= \j ->
        if j < 0 then pure i else readArray d j >>= \k ->
            if k < 0 then pure j else writeArray d i k >> go k

sameSetD :: MArray a Int m => a Int Int -> Int -> Int -> m Bool
sameSetD d i j = (==) <$> findD d i <*> findD d j

unionD :: MArray a Int m => a Int Int -> Int -> Int -> m Bool
unionD d i j = join (go <$> findD d i <*> findD d j) where
    go i j
        | i == j    = pure False
        | otherwise = True <$ join (upd i j <$> readArray d i <*> readArray d j)
    upd i j si sj
        | si > sj   = upd j i sj si
        | otherwise = writeArray d i (si + sj) >> writeArray d j i

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE findD #-}
{-# INLINABLE sameSetD #-}
{-# INLINABLE unionD #-}
