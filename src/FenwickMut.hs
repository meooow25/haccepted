{-
Mutable Fenwick tree, or binary indexed tree

A data structure supporting point updates and range queries, or the opposite.
See Fenwick.hs for a purely functional version. FenwickMut is multiple times faster when used with
unboxed arrays (see benchmarks).

Sources:
* Peter M. Fenwick, "A New Data Structure for Cumulative Frequency Tables", 1994
  https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.8917
* https://en.wikipedia.org/wiki/Fenwick_tree

Let n = r - l + 1 where (l, r) is the range of the Fenwick tree.
The complexities assume (<>) takes O(1) time.

emptyFM
Builds a Fenwick tree on range (l, r) where each element is mempty. O(n).

mappendFM
mappends to the element at an index. O(log n).

foldPrefixFM
The result of folding the prefix upto the given index. Indices outside the tree range are allowed,
it is assumed elements there are mempty. O(log n).

foldRangeFM
Folds the elements in the range (l, r). O(log n).

mappendRangeFM
mappends to all elements in the range (l, r). Can be used with foldPrefixFM for point queries.
O(log n).
-}

module FenwickMut
    ( FenwickMut
    , emptyFM
    , mappendFM
    , foldPrefixFM
    , foldRangeFM
    , mappendRangeFM
    ) where

import Control.Monad
import Data.Array.Base
import Data.Bits

import Misc ( Commutative, Group(..), modifyArray' )

type FenwickMut marr a = marr Int a

emptyFM :: (Monoid a, MArray marr a m) => (Int, Int) -> m (FenwickMut marr a)
emptyFM = flip newArray mempty

mappendFM :: (Monoid a, MArray marr a m) => FenwickMut marr a -> Int -> a -> m ()
mappendFM a i x = do
    (l,r) <- getBounds a
    mapM_ (\j -> modifyArray' a (j+l-1) (<> x)) $
        takeWhile (<= r-l+1) $ iterate (\j -> j + j .&. (-j)) (i-l+1)

foldPrefixFM :: (Monoid a, MArray marr a m) => FenwickMut marr a -> Int -> m a
foldPrefixFM a i = do
    (l,r) <- getBounds a
    foldM (\z j -> (<> z) <$!> readArray a (j+l-1)) mempty $
        takeWhile (>0) $ iterate (\j -> j - j .&. (-j)) (min i r - l + 1)

foldRangeFM :: (Commutative a, Group a, MArray marr a m) => FenwickMut marr a -> Int -> Int -> m a
foldRangeFM a l r = do
    lx <- foldPrefixFM a (l-1)
    rx <- foldPrefixFM a r
    pure $! invert lx <> rx

mappendRangeFM :: (Commutative a, Group a, MArray marr a m)
               => FenwickMut marr a -> Int -> Int -> a -> m ()
mappendRangeFM a l r x = do
    (_,r0) <- getBounds a
    mappendFM a l x
    when (r < r0) $ mappendFM a (r+1) (invert x)

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE mappendFM #-}
{-# INLINABLE foldPrefixFM #-}
