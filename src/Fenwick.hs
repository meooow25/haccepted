{-|
Fenwick tree, or binary indexed tree

A data structure supporting point updates and range queries, or the opposite.
Large ranges, beyond typical memory limits, are supported.
See FenwickMut.hs for a mutable (and more commonly seen) version.

Sources:
* https://en.wikipedia.org/wiki/Fenwick_tree
* Peter M. Fenwick, "A New Data Structure for Cumulative Frequency Tables", 1994
  https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.14.8917
* https://hackage.haskell.org/package/binary-indexed-tree

Implementation notes:
* The implementation here is literally a tree, unlike the usual implementation with an array.
  The responsibilies of the indices remain the same.
* It is a complete binary tree where each node stores the accumulation of values in its left
  subtree and itself.

     4
    / \
   /   \
  2     6
 / \   / \
1   3 5   7

Let n = r - l + 1 where (l, r) is the range of the Fenwick tree.
The complexities assume (<>) takes O(1) time.

emptyF
Builds a Fenwick tree on range (l, r) where each element is mempty. O(log n).

fromListF
Builds a Fenwick tree on (l, r) where the elements are taken from a list. If the list is shorter
than the range, the remaining elements are mempty. O(n).

mappendF
mappends to the element at an index. O(log n).

foldPrefixF
The result of folding the prefix upto the given index. Indices outside the tree range are allowed,
it is assumed elements there are mempty. O(log n).

foldRangeF
Folds the elements in the range (l, r). O(log n).

mappendRangeF
mappends to all elements in the range (l, r). Can be used with foldPrefixF for point queries.
O(log n).

binSearchF
Binary searches for the shortest prefix such that the fold of all values in it satisfies the given
monotonic predicate. Returns the end index and the fold of the found prefix. O(log n).

toScanl1F
Converts to a list of prefix accumulated values. O(n).
-}

module Fenwick
    ( FTree
    , emptyF
    , fromListF
    , mappendF
    , foldPrefixF
    , foldRangeF
    , mappendRangeF
    , binSearchF
    , toScanl1F
    ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.State
import Data.Bits

import Misc ( Commutative, Group(..), bitLength )

data FTree a = FTree !(Int, Int, Int) !(FNode a) deriving Show
data FNode a = FTip | FBin !a !(FNode a) !(FNode a) deriving Show

buildF :: Monoid a => (Int, Int) -> (Int -> FNode a) -> FTree a
buildF (l, r) _ | l > r + 1 = error "buildF: invalid range"
buildF (l, r) f = FTree (l, r, ht) (f ht)
  where
    n = r - l + 1
    ht = bitLength n - 1

emptyF :: Monoid a => (Int, Int) -> FTree a
emptyF bnds = buildF bnds go where
    go (-1) = FTip
    go j    = FBin mempty lr lr where lr = go (j - 1)

fromListF :: Monoid a => (Int, Int) -> [a] -> FTree a
fromListF bnds xs = buildF bnds (fst . flip evalState xs . go) where
    pop = state go where
        go []     = (mempty, [])
        go (x:xs) = (x,      xs)
    go (-1) = pure (FTip, mempty)
    go j = do
        (lt, lx) <- go (j - 1)
        x <- pop
        (rt, rx) <- go (j - 1)
        let x'  = lx <> x
            x'' = x' <> rx
            n   = FBin x' lt rt
        x'' `seq` n `seq` pure (n, x'')

mappendF :: Commutative a => Int -> a -> FTree a -> FTree a
mappendF i y (FTree lrh@(l, r, ht) rt)
    | i < l || r < i = error "mappendF: outside range"
    | otherwise      = FTree lrh (go rt ht)
  where
    i' = i - l + 1
    h' = countTrailingZeros i'
    go (FBin x l r) h
        | h == h'      = FBin (x <> y) l r
        | testBit i' h = FBin x l (go r (h - 1))
        | otherwise    = FBin (x <> y) (go l (h - 1)) r
    go FTip _ = error "unexpected"

foldPrefixF :: Monoid a => Int -> FTree a -> a
foldPrefixF i (FTree (l, r, ht) rt) = if i' == 0 then mempty else go rt ht mempty where
    i' = max 0 (min r i - l + 1)
    h' = countTrailingZeros i'
    go (FBin x l r) h acc
        | h == h'      = acc <> x
        | testBit i' h = go r (h - 1) $! acc <> x
        | otherwise    = go l (h - 1) acc
    go FTip _ _ = error "unexpected"

foldRangeF :: (Commutative a, Group a) => Int -> Int -> FTree a -> a
foldRangeF l r ft = foldPrefixF r ft <> invert (foldPrefixF (l - 1) ft)

mappendRangeF :: (Commutative a, Group a) => Int -> Int -> a -> FTree a -> FTree a
mappendRangeF l r y ft@(FTree (_, r', _) _) = ft'' where
    ft' = mappendF l y ft
    ft'' = if r == r' then ft' else mappendF (r + 1) (invert y) ft'

binSearchF :: Monoid a => (a -> Bool) -> FTree a -> Maybe (Int, a)
binSearchF f (FTree (l, _, ht) rt) = go rt ht (l - 1) mempty where
    go FTip _ _ _ = Nothing
    go (FBin x l r) h i acc
        | f acc'    = i' `seq` go l (h - 1) i acc <|> Just (i', acc')
        | otherwise = i' `seq` go r (h - 1) i' acc'
      where
        acc' = acc <> x
        i' = i + bit h

toScanl1F :: Monoid a => FTree a -> [a]
toScanl1F (FTree (l, r, _) rt) = take (r - l + 1) $ go rt mempty [] where
    go FTip         _   = id
    go (FBin x l r) acc = go l acc . (acc':) . go r acc' where acc' = acc <> x

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE fromListF #-}
{-# INLINABLE mappendF #-}
{-# INLINABLE foldPrefixF #-}
{-# INLINABLE binSearchF #-}

instance NFData a => NFData (FTree a) where
    rnf (FTree lrh rt) = rnf lrh `seq` rnf rt

instance NFData a => NFData (FNode a) where
    rnf FTip = ()
    rnf (FBin x l r) = rnf x `seq` rnf l `seq` rnf r
