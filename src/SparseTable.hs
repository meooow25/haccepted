{-|
Sparse table

Structure for range queries

Sources:
* https://cp-algorithms.com/data_structures/sparse-table.html
* https://github.com/kth-competitive-programming/kactl/blob/main/content/data-structures/RMQ.h

fromArraySP
Construct a sparse table from an Array Int. O(n log n).

fromListSP
Construct a sparse table from a list. O(n log n).

querySP
Query a range [l, r]. O(log n), or more accurately O(popcount (r - l + 1)).

query1SP
Query a range [l, r] when x <> x = x. O(1).

querymSP
querySP but works on an empty range.

querym1SP
query1SP but works on an empty range.
-}

module SparseTable
    ( SparseTable
    , fromArraySP
    , fromListSP
    , querySP
    , querymSP
    , query1SP
    , querym1SP
    ) where

import Data.Array
import Data.Bits

import Misc ( fArray )

type SparseTable a = Array Int (Array Int a)

fromArraySP :: Semigroup a => Array Int a -> SparseTable a
fromArraySP a = if l > h + 1 then error "invalid range" else t where
    (l, h) = bounds a
    n = h - l + 1
    k = finiteBitSize n - countLeadingZeros n - 1
    t = fArray (0, k) f
    f j | j == 0    = a
        | otherwise = fArray (l, h - 2 * hf + 1) g
        where hf = 1 `shiftL` (j - 1)
              p = t!(j - 1)
              g i = p!i <> p!(i + hf)

fromListSP :: Semigroup a => (Int, Int) -> [a] -> SparseTable a
fromListSP = (fromArraySP .) . listArray

querySP :: Semigroup a => Int -> Int -> SparseTable a -> a
querySP l r _ | l > r = error "invalid range"
querySP l r t = go l where
    r' = r + 1
    go l | l' == r'  = t!j!l
         | otherwise = t!j!l <> go l'
         where j = countTrailingZeros $ r' - l
               l' = l + 1 `shiftL` j

query1SP :: Semigroup a => Int -> Int -> SparseTable a -> a
query1SP l r _ | l > r = error "invalid range"
query1SP l r t = t!k!l <> t!k!l' where
    n = r - l + 1
    k = finiteBitSize n - countLeadingZeros n - 1
    l' = r + 1 - 1 `shiftL` k

querymSP :: Monoid a => Int -> Int -> SparseTable a -> a
querymSP l r t = if l == r + 1 then mempty else querySP l r t

querym1SP :: Monoid a => Int -> Int -> SparseTable a -> a
querym1SP l r t = if l == r + 1 then mempty else query1SP l r t

-- TODO: querySP can be made tail recursive, if it's worth it

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE fromArraySP #-}
{-# INLINABLE fromListSP #-}
{-# INLINABLE querySP #-}
{-# INLINABLE query1SP #-}
