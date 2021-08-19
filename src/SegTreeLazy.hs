{-# LANGUAGE MultiParamTypeClasses #-}
{-
Segment tree with lazy propagation

A data structure supporting point updates, range queries, and certain range updates on a sequence of
monoids. This differs from an ordinary segment tree in its ability to apply updates on a range, they
are otherwise identical.
This implementation, like SegTree, supports ranges that do not fit in memory.

Sources:
* https://cp-algorithms.com/data_structures/segment_tree.html

Implementation notes:
See SegTree.hs because the structure is identical. The only difference is that each node holds an
update here.

LazySegTree u a is a segment tree on elements of type a and updates of type u. a and u must be
monoids. An instance of LazySegTreeUpd u a must exist, where applyUpd a u applies u to a.
The following must hold:
* (x1 <> x2) `applyUpd` u = (x1 `applyUpd` u) <> (x2 `applyUpd` u)
* x `applyUpd` (u1 <> u2) = (x `applyUpd` u1) `applyUpd` u2

The complexities below assume mappend for u, mappend for a and applyUpd all take O(1) time.
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
    , LazySegTreeUpd(..)
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

class (Monoid u, Monoid a) => LazySegTreeUpd u a where
    applyUpd :: a -> u -> a

buildLST :: LazySegTreeUpd u a => (Int, Int) -> (Int -> LSegNode u a) -> LazySegTree u a
buildLST (l, r) f
    | n < -1    = error "invalid range"
    | n == -1   = LazySegTree (l, r, 0) $ LSLeaf mempty
    | otherwise = LazySegTree (l, r, bit ht) $ f ht
  where
    n = r - l
    ht = finiteBitSize n - countLeadingZeros n

emptyLST :: LazySegTreeUpd u a => (Int, Int) -> LazySegTree u a
emptyLST bnds = buildLST bnds go where
    go j | j == 0 = LSLeaf mempty
    go j = LSBin mempty mempty lr lr where lr = go $ j - 1

makeLSN :: LazySegTreeUpd u a => LSegNode u a -> LSegNode u a -> LSegNode u a
makeLSN lt rt = LSBin (getx lt <> getx rt) mempty lt rt where
    getx (LSLeaf x) = x
    getx (LSBin x _ _ _) = x

fromListLST :: LazySegTreeUpd u a => (Int, Int) -> [a] -> LazySegTree u a
fromListLST bnds xs = buildLST bnds (flip evalState xs . go) where
    pop = state go where
        go []     = (mempty, [])
        go (x:xs) = (x,      xs)
    go j | j == 0 = LSLeaf <$> pop
    go j = makeLSN <$> go (j - 1) <*> go (j - 1)

boundsLST :: LazySegTree u a -> (Int, Int)
boundsLST (LazySegTree (l, r, _) _) = (l, r)

applyLSN :: LazySegTreeUpd u a => LSegNode u a -> u -> LSegNode u a
applyLSN (LSLeaf x)         u = LSLeaf $ applyUpd x u
applyLSN (LSBin x u' lt rt) u = LSBin (applyUpd x u) (u' <> u) lt rt

adjustLST :: LazySegTreeUpd u a => (a -> a) -> Int -> LazySegTree u a -> LazySegTree u a
adjustLST f i (LazySegTree lrp@(l, r, p) root)
    | i < l || r < i = error "outside range"
    | otherwise      = LazySegTree lrp $ go root l (l + p - 1) mempty
  where
    go n l r pu | i < l || r < i = applyLSN n pu
    go (LSLeaf x)        _ _ pu = LSLeaf $ f $ applyUpd x pu
    go (LSBin _ u lt rt) l r pu = makeLSN (go lt l m u') (go rt (m + 1) r u') where
        m = (l + r) `div` 2
        u' = u <> pu

updateRangeLST :: LazySegTreeUpd u a => u -> Int -> Int -> LazySegTree u a -> LazySegTree u a
updateRangeLST u ql qr (LazySegTree lrp@(l, r, p) root)
    | ql < l || r < qr = error "outside range"
    | otherwise        = LazySegTree lrp $ go root l (l + p - 1) mempty
  where
    go n l r pu
        | r < ql || qr < l   = applyLSN n pu
        | ql <= l && r <= qr = applyLSN n (pu <> u)
    go ~(LSBin _ u' lt rt) l r pu = makeLSN (go lt l m u'') (go rt (m + 1) r u'') where
        m = (l + r) `div` 2
        u'' = u' <> pu

foldRangeLST :: LazySegTreeUpd u a => Int -> Int -> LazySegTree u a -> a
foldRangeLST ql qr _ | ql > qr + 1 = error "invalid range"
foldRangeLST ql qr (LazySegTree (l, _, p) root) = go root l (l + p - 1) mempty mempty where
    go _ l r _ acc | r < ql || qr < l = acc
    go (LSLeaf x) _ _ pu acc = acc <> applyUpd x pu
    go (LSBin x u lt rt) l r pu acc
        | ql <= l && r <= qr = acc <> applyUpd x pu
        | otherwise          = go rt (m + 1) r u' $! go lt l m u' acc
      where
        m = (l + r) `div` 2
        u' = u <> pu

toListLST :: LazySegTreeUpd u a => LazySegTree u a -> [a]
toListLST (LazySegTree (l, r', p) root) = go root l (l + p - 1) mempty [] where
    go _ l _ _ acc | l > r' = acc
    go (LSLeaf x)        _ _ pu acc = applyUpd x pu : acc
    go (LSBin _ u lt rt) l r pu acc = go lt l m u' $ go rt (m + 1) r u' acc where
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
