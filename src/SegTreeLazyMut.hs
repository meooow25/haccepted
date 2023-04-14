{-|
Mutable segment tree with lazy propagation

See LazySegTree. LazySegTreeMut is just that, but backed by mutable arrays.
When the arrays are unboxed, LazySegTreeMut is a few times faster than LazySegTree (see benchmarks).
However, this comes at the cost of purity.

Implementation notes:
* INLINE on setLSNM and applyLSNM is critical!

emptyLSTM
Builds a segment tree on range (l, r) where each element is mempty. O(n).

fromListLSTM
Builds a segment tree on (l, r) where the elements are taken from a list. If the list is shorter
than the range, the remaining elements are mempty. O(n).

adjustLSTM
Adjusts the element at index i. O(log n).

updateRangeLSTM
Applies an update on elements in the range (ql, qr). O(log n).

foldRangeLSTM
Folds the elements in the range (ql, qr). Elements outside (l, r) are considered to be mempty.
O(log n).

foldrLSTM
Right fold over the elements of the segment tree. O(n).
-}

module SegTreeLazyMut
    ( LazySegTreeMut
    , emptyLSTM
    , fromListLSTM
    , adjustLSTM
    , updateRangeLSTM
    , foldRangeLSTM
    , foldrLSTM
    ) where

import Control.Monad.State
import Data.Array.MArray

import Misc ( modifyArray', Action(..) )

data LazySegTreeMut marru marra u a = LSTM !Int !Int !(marru Int u) !(marra Int a)

emptyLSTM :: (Action u a, MArray marru u m, MArray marra a m)
          => (Int, Int) -> m (LazySegTreeMut marru marra u a)
emptyLSTM (l,r) | l > r + 1 = error "emptyLSTM: bad range"
emptyLSTM (l,r) = do
    let n = r - l + 1
    ua <- newArray (1, 4*n) mempty
    aa <- newArray (1, 4*n) mempty
    pure $! LSTM l r ua aa

setLSNM :: (Action u a, MArray marru u m, MArray marra a m)
        => marru Int u -> marra Int a -> Int -> m ()
setLSNM ua aa i = do
    writeArray ua i $! mempty
    (<>) <$> readArray aa (2*i) <*> readArray aa (2*i+1) >>= (writeArray aa i $!)
{-# INLINE setLSNM #-}

fromListLSTM :: (Action u a, MArray marru u m, MArray marra a m)
             => (Int, Int) -> [a] -> m (LazySegTreeMut marru marra u a)
fromListLSTM (l0,r0) _ | l0 > r0 + 1 = error "fromListLSTM: bad range"
fromListLSTM (l0,r0) xs = do
    let n = r0 - l0 + 1
    ua <- newArray (1, 4*n) mempty
    aa <- newArray (1, 4*n) mempty
    let pop = StateT go' where
            go' []     = pure (mempty, [])
            go' (y:ys) = pure (y,      ys)
        go i l r | l == r = pop >>= lift . (writeArray aa i $!)
        go i l r = do
            let m = (l+r) `div` 2
            go (2*i) l m
            go (2*i+1) (m+1) r
            lift (setLSNM ua aa i)
    when (n > 0) $ evalStateT (go 1 l0 r0) xs
    pure $! LSTM l0 r0 ua aa

applyLSNM :: (Action u a, MArray marru u m, MArray marra a m)
          => marru Int u -> marra Int a -> Int -> Int -> Int -> u -> m ()
applyLSNM ua aa i l r u
    | l == r    = modifyArray' aa i (`act` u)
    | otherwise = modifyArray' aa i (`act` u) *> modifyArray' ua i (<> u)
{-# INLINE applyLSNM #-}

adjustLSTM :: (Action u a, MArray marru u m, MArray marra a m)
           => LazySegTreeMut marru marra u a -> Int -> (a -> a) -> m ()
adjustLSTM (LSTM l0 r0 ua aa) qi f
    | qi < l0 || r0 < qi = error "adjustLSTM: outside range"
    | otherwise          = go 1 l0 r0 mempty
  where
    go i l r pu
        | qi < l || r < qi = applyLSNM ua aa i l r pu
        | l == r           = modifyArray' aa i (f . (`act` pu))
        | otherwise = do
            u <- readArray ua i
            let m = (l+r) `div` 2
                u' = u <> pu
            go (2*i) l m u'
            go (2*i+1) (m+1) r u'
            setLSNM ua aa i

updateRangeLSTM :: (Action u a, MArray marru u m, MArray marra a m)
                => LazySegTreeMut marru marra u a -> Int -> Int -> u -> m ()
updateRangeLSTM (LSTM l0 r0 ua aa) ql qr qu
    | ql > qr + 1        = error "updateRangeLSTM: bad range"
    | ql < l0 || r0 < qr = error "updateRangeLSTM: outside range"
    | otherwise          = go 1 l0 r0 mempty
  where
    go i l r pu
        | r < ql || qr < l   = applyLSNM ua aa i l r pu
        | ql <= l && r <= qr = applyLSNM ua aa i l r (pu <> qu)
        | otherwise = do
            u <- readArray ua i
            let m = (l+r) `div` 2
                u' = u <> pu
            go (2*i) l m u'
            go (2*i+1) (m+1) r u'
            setLSNM ua aa i

foldRangeLSTM :: (Action u a, MArray marru u m, MArray marra a m)
              => LazySegTreeMut marru marra u a -> Int -> Int -> m a
foldRangeLSTM (LSTM l0 r0 ua aa) ql qr
    | ql > qr + 1 = error "foldRangeLSTM: bad range"
    | l0 > r0     = pure mempty
    | otherwise   = go 1 l0 r0 mempty mempty
  where
    go i l r pu acc
        | r < ql || qr < l   = pure acc
        | ql <= l && r <= qr = (acc <>) . (`act` pu) <$!> readArray aa i
        | otherwise = do
            u <- readArray ua i
            let m = (l+r) `div` 2
                u' = u <> pu
            go (2*i) l m u' acc >>= go (2*i+1) (m+1) r u'

foldrLSTM :: (Action u a, MArray marru u m, MArray marra a m)
          => LazySegTreeMut marru marra u a -> (a -> b -> b) -> b -> m b
foldrLSTM (LSTM l0 r0 ua aa) f z0
    | l0 > r0   = pure z0
    | otherwise = go 1 l0 r0 mempty z0
  where
    go i l r pu z
        | l == r = (`f` z) . (`act` pu) <$> readArray aa i
        | otherwise = do
            u <- readArray ua i
            let m = (l+r) `div` 2
                u' = u <> pu
            go (2*i+1) (m+1) r u' z >>= go (2*i) l m u'

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE fromListLSTM #-}
{-# INLINABLE adjustLSTM #-}
{-# INLINABLE updateRangeLSTM #-}
{-# INLINABLE foldRangeLSTM #-}
