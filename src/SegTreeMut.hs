{-# LANGUAGE BangPatterns #-}
{-|
Mutable segment tree

See SegTree. SegTreeMut is just that, but backed by a mutable array.
With unboxed arrays, SegTreeMut is multiple times faster than SegTree (see benchmarks).
However, this comes at the cost of purity.

Sources:
* Al.Cash, "Efficient and easy segment trees"
  https://codeforces.com/blog/entry/18051
* AtCoder Library
  https://github.com/atcoder/ac-library/blob/master/atcoder/segtree.hpp

Implementation notes:
* The implementation here is a bottom-up implementation, sometimes called "iterative" segment tree.
  A top-down implementation also works, but is generally slower.
* The values of the tree are stored in an Int-indexed array. Nodes are represented by Ints, which
  are the indices where the value of the node is stored. The value of the root is stored at index 1.
  The childen of node i are 2*i and 2*i+1. Each node covers a range the size of which is a power of
  2, and the two children of the node, if it's not a leaf, cover the two halves of that range.
* Operations on the tree proceed bottom-up, from the leaves towards the root. Due to the uniform
  structure of the tree it is easy and efficient to traverse the tree by manipulating indices.
* INLINE on setSNM is critical!
* The "unsafe" bitwise ops are safe in the given conditions. For instance,
  x `unsafeShiftR` (ctz (x+1)) is safe if x is not -1. These are used only where it will be run many
  many times and will make a difference.

Let n = r - l + 1 where (l, r) is the range of the segment tree.
The complexities assume (<>) takes O(1) time.

emptySTM
Builds a segment tree on (l, r) where each element is mempty. O(n).

fromListSTM
Builds a segment tree on (l, r) where the elements are taken from a list. If the list is shorter
than the range, the remaining elements are mempty. O(n).

adjustSTM
Adjusts the element at index i. O(log n).

foldRangeSTM
Folds the elements in the range (ql, qr). Elements outside (l, r) are considered to be mempty.
O(log n), or more precisely O(log(min(r, qr) - max(l, ql) + 1)).

binSearchSTM
Binary search in the intersection of (l, r) and (ql, qr) for the shortest prefix whose fold
satisfies the given monotonic predicate. Returns the end index and the fold. O(log n), or more
precisely O(log(min(r, qr) - max(l, ql) + 1)).

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

import Control.Monad
import Data.Array.MArray
import Data.Bits
import Data.List

import Misc ( bitLength, modifyArray', unsafeBit )

data SegTreeMut marr a = STM !Int !Int !Int !(marr Int a)

emptySTM :: (Monoid a, MArray marr a m) => (Int, Int) -> m (SegTreeMut marr a)
emptySTM (l,r)
    | r + 1 < l = error "emptySTM: bad range"
    | otherwise = do
        let n = bit (bitLength (r - l))
        xa <- newArray (1, 2*n-1) mempty
        pure $! STM l r n xa

setSNM :: (Monoid a, MArray marr a m) => marr Int a -> Int -> m ()
setSNM xa = \i -> (<>) <$> readArray xa (2*i) <*> readArray xa (2*i+1) >>= (writeArray xa i $!)
{-# INLINE setSNM #-}

fromListSTM :: (Monoid a, MArray marr a m) => (Int, Int) -> [a] -> m (SegTreeMut marr a)
fromListSTM (l,r) xs
    | r + 1 < l = error "fromListSTM: bad range"
    | otherwise = do
        let n = bit (bitLength (r - l))
        xa <- newArray (1, 2*n-1) mempty
        forM_ (zip [n .. n + r - l] xs) $ uncurry (writeArray xa)
        forM_ [n-1, n-2 .. 1] $ setSNM xa
        pure $! STM l r n xa

adjustSTM :: (Monoid a, MArray marr a m) => SegTreeMut marr a -> Int -> (a -> a) -> m ()
adjustSTM (STM l r n xa) qi f
    | qi < l || r < qi = error "adjustSTM: outside range"
    | otherwise = do
        let qi' = qi - l + n
        modifyArray' xa qi' f
        mapM_ (setSNM xa) $ takeWhile (>0) $ iterate' (`quot` 2) (qi' `quot` 2)

foldRangeSTM :: (Monoid a, MArray marr a m) => SegTreeMut marr a -> Int -> Int -> m a
foldRangeSTM (STM l0 r0 n xa) ql qr
    | qr + 1 < ql = error "foldRangeSTM: bad range"
    | qr' < ql'   = pure mempty
    | otherwise = do
        accL <- goUpRt (ql' - l0 + n) ql' 0 mempty
        accR <- goUpLt (qr' - l0 + n) qr' 0 mempty
        pure $! accL <> accR
  where
    ql' = max l0 ql
    qr' = min r0 qr
    goUpRt !i1 !l !d1 !acc = do
        let tz = countTrailingZeros i1
            i = i1 `unsafeShiftR` tz
            d = d1 + tz
        if qr' < l + unsafeBit d - 1
        then pure acc
        else readArray xa i >>= goUpRt (i+1) (l + unsafeBit d) d . (acc <>)
    goUpLt !j1 !r !d1 !acc = do
        let to = countTrailingZeros (j1+1)
            j = j1 `unsafeShiftR` to
            d = d1 + to
        if r - unsafeBit d + 1 < ql'
        then pure acc
        else readArray xa j >>= goUpLt (j-1) (r - unsafeBit d) d . (<> acc)

binSearchSTM :: (Monoid a, MArray marr a m)
             => SegTreeMut marr a -> Int -> Int -> (a -> Bool) -> m (Maybe (Int, a))
binSearchSTM (STM l0 r0 n xa) ql qr p
    | qr + 1 < ql = error "binSearchSTM: bad range"
    | qr' < ql'   = pure Nothing
    | otherwise   = goUpRt (ql' - l0 + n) ql' 0 mempty
  where
    ql' = max l0 ql
    qr' = min r0 qr
    goUpRt !i1 !l !d1 !acc = do
        let tz = countTrailingZeros i1
            i = i1 `unsafeShiftR` tz
            d = d1 + tz
        !acc' <- (acc <>) <$> readArray xa i
        case () of
            _ | p acc'                 -> goDn i l d acc
              | l + unsafeBit d <= qr' -> goUpRt (i+1) (l + unsafeBit d) d acc'
              | otherwise              -> pure Nothing
    goDn !i !l !d !acc
        | qr' < l = pure Nothing
        | i < n = do
            !acc' <- (acc <>) <$> readArray xa (2*i)
            if p acc'
            then goDn (2*i) l (d-1) acc
            else goDn (2*i+1) (l + unsafeBit (d-1)) (d-1) acc'
        | otherwise = do
            !acc' <- (acc <>) <$> readArray xa i
            pure (Just (l, acc'))

foldrSTM :: (Monoid a, MArray marr a m) => SegTreeMut marr a -> (a -> b -> b) -> b -> m b
foldrSTM (STM l r n xa) f z = foldr (liftM2 f . readArray xa) (pure z) [n .. n + r - l]

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE fromListSTM #-}
{-# INLINABLE adjustSTM #-}
{-# INLINABLE foldRangeSTM #-}
{-# INLINABLE binSearchSTM #-}
