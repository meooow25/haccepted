{-|
Fenwick tree or binary indexed tree

Persistent version, which is a little different (and less efficient) than the standard
implementation with an array. The responsibilies of the indices are identical to the standard
version.

The tree is represented as a complete BST where each node stores the sum of values in its left
subtree and itself.

     4
    / \
   /   \
  2     6
 / \   / \
1   3 5   7

Sources:
* https://en.wikipedia.org/wiki/Fenwick_tree
* https://hackage.haskell.org/package/binary-indexed-tree

buildF
Builds a Fenwick tree with the given bounds. O(n).

boundsF
The bounds of the Fenwick tree. O(1).

updateF
Updates an index. The new value acts as right operand on accumulated values. O(log n).

queryF
Queries the prefix upto an index. O(log n).

rangeQueryF
Range query on [l, r] using an inverse operation. O(log n).

rangeUpdateF
Range update on [l, r] using an inverse operation. Can be used with point queries. O(log n).
-}

module Fenwick
    ( FTree
    , buildF
    , boundsF
    , updateF
    , queryF
    , rangeQueryF
    , rangeUpdateF
    ) where

import Data.Bits

data FTree a = FTree !(Int, Int, Int) !(FNode a) deriving Show
data FNode a = FTip | FBin !a !(FNode a) !(FNode a) deriving Show

buildF :: Monoid a => (Int, Int) -> FTree a
buildF (l, r) = FTree (l, r, bit ht) (go ht) where
    n = r - l + 1
    ht = finiteBitSize n - countLeadingZeros n - 1
    go j | j < 0     = FTip
         | otherwise = FBin mempty (go $ j - 1) (go $ j - 1)

boundsF :: FTree a -> (Int, Int)
boundsF (FTree (l, r, _) _) = (l, r)

{-# INLINE adjust #-}
adjust :: Int -> Int -> Int -> Int
adjust l r i
    | i < l || r < i = error "outside range"
    | otherwise = i - l + 1

updateF :: Monoid a => Int -> a -> FTree a -> FTree a
updateF i y (FTree lrp@(l, r, p) rt) = FTree lrp (go rt p) where
    i' = adjust l r i
    q = bit $ countTrailingZeros i'
    go ~(FBin x l r) p
        | i' .&. p == 0 = FBin (x <> y) (go l p') r
        | p == q        = FBin (x <> y) l r
        | otherwise     = FBin x l (go r p')
        where p' = p `shiftR` 1

queryF :: Monoid a => Int -> FTree a -> a
queryF i (FTree (l, r, p) rt) = go rt p mempty where
    i' = adjust l r i
    q = bit $ countTrailingZeros i'
    go ~(FBin x l r) p acc
        | i' .&. p == 0 = go l p' acc
        | p == q        = acc <> x
        | otherwise     = go r p' $! acc <> x
        where p' = p `shiftR` 1

rangeQueryF :: Monoid a => (a -> a) -> Int -> Int -> FTree a -> a
rangeQueryF inv l r ft@(FTree (l', _, _) _) = rx <> inv lx where
    rx = queryF r ft
    lx = if l == l' then mempty else queryF (l - 1) ft

rangeUpdateF :: Monoid a => (a -> a) -> Int -> Int -> a -> FTree a -> FTree a
rangeUpdateF inv l r y ft@(FTree (_, r', _) _) = ft'' where
    ft' = updateF l y ft
    ft'' = if r == r' then ft' else updateF (r + 1) (inv y) ft'
