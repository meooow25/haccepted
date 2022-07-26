{-
Segment tree

A data structure supporting point updates and range queries on a sequence of monoids.
This implementation supports large ranges that do not fit in memory, sometimes called a dynamic or
sparse segment tree.

Sources:
* https://cp-algorithms.com/data_structures/segment_tree.html

Implementation notes:
* The segment tree is implemented as a complete binary tree. This uses more memory when the range
  size is away from the next power of 2, and the full tree is actually constructed, but the symmetry
  allows for an easy O(log n) construction of an empty tree.
* It is possible to build an empty segment tree in O(1) instead of O(log n). This can be done with
  special nodes that indicate that they can be grown, but this complicates the implementation.
  A lazy spine may also give an equivalent result, but structures with strict spines perform better
  in general. The O(log n) implementation seems like a good compromise to me.
* fromListST can be made to run in something like O(length xs + log n) instead of O(n) by falling
  back to the empty tree strategy when the list is exhausted. This will also reduce the memory usage
  mentioned in point 1, but I don't think it is worth the hassle.
* If the range is larger than 32-bits and this is to be run on a 32-bit system, you can replace Int
  with Int64.

The complexities below assume mappend takes O(1) time.
Let n = r - l + 1 in all instances below.

emptyST
Builds a segment tree on range (l, r) where each element is mempty. O(log n).

fromListST
Builds a segment tree on (l, r) where the elements are taken from a list. If the list is shorter
than the range, the remaining elements are mempty. O(n).

boundsST
The bounds of the segment tree. O(1).

adjustST
Adjusts the element at index i. O(log n).

foldRangeST
Folds the elements in the range (ql, qr). Elements outside (l, r) are considered to be mempty.
O(log n).

SegTree implementable Foldable. foldMap takes O(n).
-}

module SegTree
    ( SegTree
    , emptyST
    , fromListST
    , boundsST
    , adjustST
    , foldRangeST
    ) where

import Control.DeepSeq
import Control.Monad.State
import Data.Bits

data SegTree a = SegTree !(Int, Int, Int) !(SegNode a) deriving Show
data SegNode a = SLeaf !a | SBin !a !(SegNode a) !(SegNode a) deriving Show

buildST :: Monoid a => (Int, Int) -> (Int -> SegNode a) -> SegTree a
buildST (l, r) f
    | n < -1    = error "invalid range"
    | n == -1   = SegTree (l, r, 0) (SLeaf mempty)
    | otherwise = SegTree (l, r, bit ht) (f ht)
  where
    n = r - l
    ht = finiteBitSize n - countLeadingZeros n

emptyST :: Monoid a => (Int, Int) -> SegTree a
emptyST bnds = buildST bnds go where
    go 0 = SLeaf mempty
    go j = SBin mempty lr lr where lr = go (j - 1)

makeSN :: Monoid a => SegNode a -> SegNode a -> SegNode a
makeSN lt rt = SBin (getx lt <> getx rt) lt rt where
    getx (SLeaf x)    = x
    getx (SBin x _ _) = x

fromListST :: Monoid a => (Int, Int) -> [a] -> SegTree a
fromListST bnds xs = buildST bnds (flip evalState xs . go) where
    pop = state go where
        go []     = (mempty, [])
        go (x:xs) = (x,      xs)
    go 0 = SLeaf <$> pop
    go j = makeSN <$> go (j - 1) <*> go (j - 1)

boundsST :: SegTree a -> (Int, Int)
boundsST (SegTree (l, r, _) _) = (l, r)

adjustST :: Monoid a => (a -> a) -> Int -> SegTree a -> SegTree a
adjustST f i (SegTree lrp@(l, r, p) root)
    | i < l || r < i = error "outside range"
    | otherwise      = SegTree lrp (go root l (l + p - 1))
  where
    go n l r | i < l || r < i = n
    go (SLeaf x)      _ _ = SLeaf (f x)
    go (SBin _ lt rt) l r = makeSN (go lt l m) (go rt (m + 1) r) where m = (l + r) `div` 2

foldRangeST :: Monoid a => Int -> Int -> SegTree a -> a
foldRangeST ql qr _ | ql > qr + 1 = error "invalid range"
foldRangeST ql qr (SegTree (l, _, p) root) = go root l (l + p - 1) mempty where
    go _ l r acc | r < ql || qr < l = acc
    go (SLeaf x) _ _ acc = acc <> x
    go (SBin x lt rt) l r acc
        | ql <= l && r <= qr = acc <> x
        | otherwise          = go rt (m + 1) r $! go lt l m acc
        where m = (l + r) `div` 2

instance Foldable SegTree where
    foldMap f (SegTree (l, r', p) root) = go root l (l + p - 1) where
        go _ l _ | l > r' = mempty
        go (SLeaf x)      _ _ = f x
        go (SBin _ lt rt) l r = go lt l m <> go rt (m + 1) r where m = (l + r) `div` 2

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE fromListST #-}
{-# INLINABLE adjustST #-}
{-# INLINABLE foldRangeST #-}

instance NFData a => NFData (SegTree a) where
    rnf (SegTree lrp n) = rnf lrp `seq` rnf n

instance NFData a => NFData (SegNode a) where
    rnf (SLeaf x)      = rnf x
    rnf (SBin x lt rt) = rnf x `seq` rnf lt `seq` rnf rt
