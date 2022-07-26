{-# LANGUAGE MultiParamTypeClasses #-}
{-
Segment tree with lazy propagation

A data structure supporting point updates, range queries, and certain range updates on a sequence of
monoids. This differs from an ordinary segment tree in its ability to apply updates on a range, they
are otherwise identical. In fact, Segtree a can be defined as LazySegTree () a.
This implementation, like SegTree, supports large ranges that may not fit in memory.

Sources:
* https://cp-algorithms.com/data_structures/segment_tree.html

Implementation notes:
See SegTree.hs because the structure is identical. The only difference is that each node holds an
update here.

LazySegTree u a is a segment tree on elements of type a and updates of type u. a and u must be
monoids. An instance of Action u a must exist, which specifies a (right) monoid action of u on a.
The following laws hold for a monoid action:
* (x `act` u1) `act` u2 = x `act` (u1 <> u2)
* x `act` mempty = x
The segment tree requires an additional law:
* (x1 <> x2) `act` u = (x1 `act` u) <> (x2 `act` u)

The complexities below assume <> for u, <> for a and act all take O(1) time.
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

updateRangeLST
Applies an update on elements in the range (ql, qr). O(log n).

foldRangeST
Folds the elements in the range (ql, qr). Elements outside (l, r) are considered to be mempty.
O(log n).

toListLST
Gets the elements of the segment tree as a list. O(n).
-}

module SegTreeLazy
    ( LazySegTree
    , Action(..)
    , emptyLST
    , fromListLST
    , boundsLST
    , adjustLST
    , updateRangeLST
    , foldRangeLST
    , toListLST
    ) where

import Control.DeepSeq
import Control.Monad.State
import Data.Bits

data LazySegTree u a = LazySegTree !(Int, Int, Int) !(LSegNode u a) deriving Show
data LSegNode u a = LSLeaf !a | LSBin !a !u !(LSegNode u a) !(LSegNode u a) deriving Show

class (Monoid u, Monoid a) => Action u a where
    act :: a -> u -> a

buildLST :: Action u a => (Int, Int) -> (Int -> LSegNode u a) -> LazySegTree u a
buildLST (l, r) f
    | n < -1    = error "invalid range"
    | n == -1   = LazySegTree (l, r, 0) (LSLeaf mempty)
    | otherwise = LazySegTree (l, r, bit ht) (f ht)
  where
    n = r - l
    ht = finiteBitSize n - countLeadingZeros n

emptyLST :: Action u a => (Int, Int) -> LazySegTree u a
emptyLST bnds = buildLST bnds go where
    go 0 = LSLeaf mempty
    go j = LSBin mempty mempty lr lr where lr = go (j - 1)

makeLSN :: Action u a => LSegNode u a -> LSegNode u a -> LSegNode u a
makeLSN lt rt = LSBin (getx lt <> getx rt) mempty lt rt where
    getx (LSLeaf x)      = x
    getx (LSBin x _ _ _) = x

fromListLST :: Action u a => (Int, Int) -> [a] -> LazySegTree u a
fromListLST bnds xs = buildLST bnds (flip evalState xs . go) where
    pop = state go where
        go []     = (mempty, [])
        go (x:xs) = (x,      xs)
    go 0 = LSLeaf <$> pop
    go j = makeLSN <$> go (j - 1) <*> go (j - 1)

boundsLST :: LazySegTree u a -> (Int, Int)
boundsLST (LazySegTree (l, r, _) _) = (l, r)

applyLSN :: Action u a => LSegNode u a -> u -> LSegNode u a
applyLSN (LSLeaf x)        u' = LSLeaf (act x u')
applyLSN (LSBin x u lt rt) u' = LSBin (act x u') (u <> u') lt rt

adjustLST :: Action u a => (a -> a) -> Int -> LazySegTree u a -> LazySegTree u a
adjustLST f i (LazySegTree lrp@(l, r, p) root)
    | i < l || r < i = error "outside range"
    | otherwise      = LazySegTree lrp (go root l (l + p - 1) mempty)
  where
    go n l r pu | i < l || r < i = applyLSN n pu
    go (LSLeaf x)        _ _ pu = LSLeaf (f (act x pu))
    go (LSBin _ u lt rt) l r pu = makeLSN (go lt l m u') (go rt (m + 1) r u') where
        m = (l + r) `div` 2
        u' = u <> pu

updateRangeLST :: Action u a => u -> Int -> Int -> LazySegTree u a -> LazySegTree u a
updateRangeLST qu ql qr (LazySegTree lrp@(l, r, p) root)
    | ql < l || r < qr = error "outside range"
    | otherwise        = LazySegTree lrp (go root l (l + p - 1) mempty)
  where
    go n l r pu
        | r < ql || qr < l   = applyLSN n pu
        | ql <= l && r <= qr = applyLSN n (pu <> qu)
    go ~(LSBin _ u lt rt) l r pu = makeLSN (go lt l m u') (go rt (m + 1) r u') where
        m = (l + r) `div` 2
        u' = u <> pu

foldRangeLST :: Action u a => Int -> Int -> LazySegTree u a -> a
foldRangeLST ql qr _ | ql > qr + 1 = error "invalid range"
foldRangeLST ql qr (LazySegTree (l, _, p) root) = go root l (l + p - 1) mempty mempty where
    go _ l r _ acc | r < ql || qr < l = acc
    go (LSLeaf x) _ _ pu acc = acc <> act x pu
    go (LSBin x u lt rt) l r pu acc
        | ql <= l && r <= qr = acc <> act x pu
        | otherwise          = go rt (m + 1) r u' $! go lt l m u' acc
      where
        m = (l + r) `div` 2
        u' = u <> pu

toListLST :: Action u a => LazySegTree u a -> [a]
toListLST (LazySegTree (l, r', p) root) = go root l (l + p - 1) mempty [] where
    go _ l _ _ | l > r' = id
    go (LSLeaf x)        _ _ pu = (act x pu :)
    go (LSBin _ u lt rt) l r pu = go lt l m u' . go rt (m + 1) r u' where
        m = (l + r) `div` 2
        u' = u <> pu

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE fromListLST #-}
{-# INLINABLE adjustLST #-}
{-# INLINABLE updateRangeLST #-}
{-# INLINABLE foldRangeLST #-}

instance (NFData u, NFData a) => NFData (LazySegTree u a) where
    rnf (LazySegTree lrp n) = rnf lrp `seq` rnf n

instance (NFData u, NFData a) => NFData (LSegNode u a) where
    rnf (LSLeaf x)        = rnf x
    rnf (LSBin x u lt rt) = rnf x `seq` rnf u `seq` rnf lt `seq` rnf rt
