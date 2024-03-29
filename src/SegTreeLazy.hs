{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Segment tree with lazy propagation

A data structure supporting point updates, range queries, and certain range updates on a sequence of
monoids. This differs from an ordinary segment tree in its ability to apply updates on a range, they
are otherwise identical. In fact, Segtree a can be defined as LazySegTree () a.
This implementation, like SegTree, supports large ranges that may not fit in memory.

Sources:
* https://cp-algorithms.com/data_structures/segment_tree.html

Implementation notes:
* See SegTree.hs because the structure is identical. The only difference is that each node holds an
  update here.

LazySegTree u a is a segment tree on elements of type a and updates of type u. The instance of
Action u a determines the behavior of the tree. In addition to Action's laws, the segment tree
requires
(x1 <> x2) `act` u = (x1 `act` u) <> (x2 `act` u)

The complexities below assume <> for u, <> for a and act all take O(1) time.
Let n = r - l + 1 in all instances below.

emptyLST
Builds a segment tree on range (l, r) where each element is mempty. O(log n).

fromListLST
Builds a segment tree on (l, r) where the elements are taken from a list. If the list is shorter
than the range, the remaining elements are mempty. O(n).

adjustLST
Adjusts the element at index i. O(log n).

updateRangeLST
Applies an update on elements in the range (ql, qr). O(log n).

foldRangeLST
Folds the elements in the range (ql, qr). Elements outside (l, r) are considered to be mempty.
O(log n).

foldrLST
Right fold over the elements of the segment tree. O(n).
LazySegTree u cannot be Foldable because of the Action constraint :(
-}

module SegTreeLazy
    ( LazySegTree
    , emptyLST
    , fromListLST
    , adjustLST
    , updateRangeLST
    , foldRangeLST
    , foldrLST
    ) where

import Control.DeepSeq
import Control.Monad.State
import Data.Bits

import Misc ( Action(..), bitLength )

data LazySegTree u a = LazySegTree !(Int, Int, Int) !(LSegNode u a) deriving Show
data LSegNode u a = LSLeaf !a | LSBin !a !u !(LSegNode u a) !(LSegNode u a) deriving Show

buildLST :: Action u a => (Int, Int) -> (Int -> LSegNode u a) -> LazySegTree u a
buildLST (l, r) f
    | n < -1    = error "invalid range"
    | n == -1   = LazySegTree (l, r, 0) (LSLeaf mempty)
    | otherwise = LazySegTree (l, r, bit ht) (f ht)
  where
    n = r - l
    ht = bitLength n

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

applyLSN :: Action u a => LSegNode u a -> u -> LSegNode u a
applyLSN (LSLeaf x)        u' = LSLeaf (act x u')
applyLSN (LSBin x u lt rt) u' = LSBin (act x u') (u <> u') lt rt

adjustLST :: Action u a => (a -> a) -> Int -> LazySegTree u a -> LazySegTree u a
adjustLST f i (LazySegTree lrp@(l0,r0,p) root)
    | i < l0 || r0 < i = error "adjustLST: outside range"
    | otherwise        = LazySegTree lrp (go root l0 (l0+p-1) mempty)
  where
    go n l r pu | i < l || r < i = applyLSN n pu
    go (LSLeaf x)        _ _ pu = LSLeaf (f (act x pu))
    go (LSBin _ u lt rt) l r pu = makeLSN (go lt l m u') (go rt (m+1) r u') where
        m = (l+r) `div` 2
        u' = u <> pu

updateRangeLST :: Action u a => u -> Int -> Int -> LazySegTree u a -> LazySegTree u a
updateRangeLST qu ql qr (LazySegTree lrp@(l0,r0,p) root)
    | ql > qr + 1        = error "updateRangeLSTM: bad range"
    | ql < l0 || r0 < qr = error "updateRangeLSTM: outside range"
    | otherwise          = LazySegTree lrp (go root l0 (l0+p-1) mempty)
  where
    go n l r pu
        | r < ql || qr < l   = applyLSN n pu
        | ql <= l && r <= qr = applyLSN n (pu <> qu)
    go (LSBin _ u lt rt) l r pu = makeLSN (go lt l m u') (go rt (m+1) r u') where
        m = (l+r) `div` 2
        u' = u <> pu
    go _ _ _ _ = error "impossible"

foldRangeLST :: Action u a => Int -> Int -> LazySegTree u a -> a
foldRangeLST ql qr (LazySegTree (l0,_,p) root)
    | ql > qr + 1 = error "foldRangeLST: bad range"
    | otherwise   = go root l0 (l0+p-1) mempty mempty
  where
    go _ l r _ acc | r < ql || qr < l = acc
    go (LSLeaf x) _ _ pu acc = acc <> act x pu
    go (LSBin x u lt rt) l r pu acc
        | ql <= l && r <= qr = acc <> act x pu
        | otherwise          = go rt (m+1) r u' $! go lt l m u' acc
      where
        m = (l+r) `div` 2
        u' = u <> pu

foldrLST :: Action u a => (a -> b -> b) -> b -> LazySegTree u a -> b
foldrLST f z (LazySegTree (l0,r0,p) root) = go root l0 (l0+p-1) mempty z where
    go _ l _ _ | l > r0 = id
    go (LSLeaf x)        _ _ pu = f (act x pu)
    go (LSBin _ u lt rt) l r pu = go lt l m u' . go rt (m+1) r u' where
        m = (l+r) `div` 2
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
