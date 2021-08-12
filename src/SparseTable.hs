{-|
Sparse table

Structure for range queries

Sources:
* https://cp-algorithms.com/data_structures/sparse-table.html
* https://github.com/kth-competitive-programming/kactl/blob/main/content/data-structures/RMQ.h

buildST
Construct a sparse table. O(n log n).

queryST
Query a range [l, r]. O(log n).

queryST1
Query a range [l, r] when x <> x = x. O(1).
-}

module SparseTable where

import Data.Array
import Data.Bits

import Misc ( fArray )

type SparseTable a = Array Int (Array Int a)

buildST :: Semigroup a => Array Int a -> SparseTable a
buildST a = t where
    (l, h) = bounds a
    n = h - l + 1
    k = finiteBitSize n - countLeadingZeros n - 1
    t = fArray (0, k) f
    f j | j == 0    = a
        | otherwise = fArray (l, h - 2 * half + 1) g
        where half = 1 `shiftL` (j - 1)
              prev = t!(j - 1)
              g i = prev!i <> prev!(i + half)

queryST :: Semigroup a => Int -> Int -> SparseTable a -> a
queryST l r _ | l > r = error "invalid range"
queryST l r t = go l $ snd $ bounds t where
    go l j
        | l' == r   = t!j!l
        | l' < r    = t!j!l <> go (l' + 1) (j - 1)
        | otherwise = go l (j - 1)
        where l' = l + 1 `shiftL` j - 1

query1ST :: Semigroup a => Int -> Int -> SparseTable a -> a
query1ST l r _ | l > r = error "invalid range"
query1ST l r t = t!k!l <> t!k!l' where
    n = r - l + 1
    k = finiteBitSize n - countLeadingZeros n - 1
    l' = r + 1 - 1 `shiftL` k

-- TODO: queryST can be made tail recursive, if it's worth it
