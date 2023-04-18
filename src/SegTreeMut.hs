{-|
Mutable segment tree

See SegTree. SegTreeMut is just that, but backed by a mutable array.
When the array is unboxed, SegTreeMut is a few times faster than SegTree (see benchmarks).
However, this comes at the cost of purity.

Implementation notes:
* INLINE on setSNM is critical!

emptySTM
Builds a segment tree on range (l, r) where each element is mempty. O(n).

fromListSTM
Builds a segment tree on (l, r) where the elements are taken from a list. If the list is shorter
than the range, the remaining elements are mempty. O(n).

adjustSTM
Adjusts the element at index i. O(log n).

foldRangeSTM
Folds the elements in the range (ql, qr). Elements outside (l, r) are considered to be mempty.
O(log n).

binSearchSTM
Binary search in the intersection of (l, r) and (ql, qr) for the shortest prefix whose fold
satisfies the given monotonic predicate. Returns the end index and the fold. O(log n).

foldrSTM
Right fold over the elements of the segment tree. O(n).
-}

module SegTreeMut
    ( SegTreeMut
    , emptySTM
    , fromListSTM
    , adjustSTM
    , foldRangeSTM
    , binSearchSTM
    , foldrSTM
    ) where

import Control.Monad.State
import Data.Array.MArray
import Data.Bits

import Misc ( bitLength, modifyArray' )

data SegTreeMut marr a = LSTM !Int !Int !(marr Int a)

emptySTM :: (Monoid a, MArray marr a m) => (Int, Int) -> m (SegTreeMut marr a)
emptySTM (l,r) | l > r + 1 = error "emptySTM: bad range"
emptySTM (l,r) = do
    let n = r - l + 1
    xa <- newArray (1, bit (1 + bitLength (n-1))) mempty
    pure $! LSTM l r xa

setSNM :: (Monoid a, MArray marr a m) => marr Int a -> Int -> m ()
setSNM xa i = (<>) <$> readArray xa (2*i) <*> readArray xa (2*i+1) >>= (writeArray xa i $!)
{-# INLINE setSNM #-}

fromListSTM :: (Monoid a, MArray marr a m) => (Int, Int) -> [a] -> m (SegTreeMut marr a)
fromListSTM (l0,r0) _ | l0 > r0 + 1 = error "fromListSTM: bad range"
fromListSTM (l0,r0) xs = do
    let n = r0 - l0 + 1
    xa <- newArray (1, bit (1 + bitLength (n-1))) mempty
    let pop = StateT go' where
            go' []     = pure (mempty, [])
            go' (y:ys) = pure (y,      ys)
        go i l r | l == r = pop >>= lift . (writeArray xa i $!)
        go i l r = do
            let m = (l+r) `div` 2
            go (2*i) l m
            go (2*i+1) (m+1) r
            lift (setSNM xa i)
    when (n > 0) $ evalStateT (go 1 l0 r0) xs
    pure $! LSTM l0 r0 xa

adjustSTM :: (Monoid a, MArray marr a m) => SegTreeMut marr a -> Int -> (a -> a) -> m ()
adjustSTM (LSTM l0 r0 xa) qi f
    | qi < l0 || r0 < qi = error "adjustSTM: outside range"
    | otherwise          = go 1 l0 r0
  where
    go i l r
        | qi < l || r < qi = pure ()
        | l == r           = modifyArray' xa i f
        | otherwise = do
            let m = (l+r) `div` 2
            go (2*i) l m
            go (2*i+1) (m+1) r
            setSNM xa i

foldRangeSTM :: (Monoid a, MArray marr a m) => SegTreeMut marr a -> Int -> Int -> m a
foldRangeSTM (LSTM l0 r0 xa) ql qr
    | ql > qr + 1 = error "foldRangeSTM: bad range"
    | l0 > r0     = pure mempty
    | otherwise   = go 1 l0 r0 mempty
  where
    go i l r acc
        | r < ql || qr < l   = pure acc
        | ql <= l && r <= qr = (acc <>) <$!> readArray xa i
        | otherwise          = let m = (l+r) `div` 2 in go (2*i) l m acc >>= go (2*i+1) (m+1) r

binSearchSTM :: (Monoid a, MArray marr a m)
              => SegTreeMut marr a -> Int -> Int -> (a -> Bool) -> m (Maybe (Int, a))
binSearchSTM (LSTM l0 r0 xa) ql qr p
    | ql > qr + 1 = error "binSearchSTM: bad range"
    | l0 > r0     = pure Nothing
    | otherwise   = either (const Nothing) Just <$> go 1 l0 r0 mempty
  where
    go i l r acc
        | r < ql || qr < l = pure (Left acc)
        | ql <= l && r <= qr = do
            a <- readArray xa i
            let acc' = acc <> a
            case () of
                _ | not (p acc') -> pure (Left acc')
                  | l == r       -> pure (Right (l, acc'))
                  | otherwise    -> goLR i l r acc
        | otherwise = goLR i l r acc
    goLR i l r acc = do
        let m = (l+r) `div` 2
        lres <- go (2*i) l m acc
        case lres of
            Left acc' -> go (2*i+1) (m+1) r acc'
            _         -> pure lres

foldrSTM :: (Monoid a, MArray marr a m) => SegTreeMut marr a -> (a -> b -> b) -> b -> m b
foldrSTM (LSTM l0 r0 xa) f z0
    | l0 > r0   = pure z0
    | otherwise = go 1 l0 r0 z0
  where
    go i l r z
        | l == r    = (`f` z) <$> readArray xa i
        | otherwise = let m = (l+r) `div` 2 in go (2*i+1) (m+1) r z >>= go (2*i) l m

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE fromListSTM #-}
{-# INLINABLE adjustSTM #-}
{-# INLINABLE foldRangeSTM #-}
{-# INLINABLE binSearchSTM #-}
