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
Builds an empty Fenwick tree with the given bounds. O(log n).

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
import Control.DeepSeq

data FTree a = FTree !(Int, Int, Int) !(FNode a) deriving Show
data FNode a = FTip | FBin !a !(FNode a) !(FNode a) deriving Show

buildF :: Monoid a => (Int, Int) -> FTree a
buildF (l, r) | l > r + 1 = error "invalid range"
buildF (l, r) = FTree (l, r, p) (go ht) where
    n = r - l + 1
    ht = finiteBitSize n - countLeadingZeros n - 1
    p = if ht < 0 then 0 else bit ht
    go j | j < 0     = FTip
         | otherwise = FBin mempty lr lr where lr = go $ j - 1

boundsF :: FTree a -> (Int, Int)
boundsF (FTree (l, r, _) _) = (l, r)

updateF :: Monoid a => Int -> a -> FTree a -> FTree a
updateF i y (FTree lrp@(l, r, p) rt) = FTree lrp (go rt p) where
    i' = if i < l || r < i then error "outside range" else i - l + 1
    q = bit $ countTrailingZeros i'
    go ~(FBin x l r) p
        | i' .&. p == 0 = FBin (x <> y) (go l p') r
        | p == q        = FBin (x <> y) l r
        | otherwise     = FBin x l (go r p')
        where p' = p `shiftR` 1

queryF :: Monoid a => Int -> FTree a -> a
queryF i (FTree (l, r, p) rt) = if i' == 0 then mempty else go rt p mempty where
    i' = max 0 $ min r i - l + 1
    q = bit $ countTrailingZeros i'
    go ~(FBin x l r) p acc
        | i' .&. p == 0 = go l p' acc
        | p == q        = acc <> x
        | otherwise     = go r p' $! acc <> x
        where p' = p `shiftR` 1

rangeQueryF :: Monoid a => (a -> a) -> Int -> Int -> FTree a -> a
rangeQueryF inv l r ft = queryF r ft <> inv (queryF (l - 1) ft)

rangeUpdateF :: Monoid a => (a -> a) -> Int -> Int -> a -> FTree a -> FTree a
rangeUpdateF inv l r y ft@(FTree (_, r', _) _) = ft'' where
    ft' = updateF l y ft
    ft'' = if r == r' then ft' else updateF (r + 1) (inv y) ft'

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE buildF #-}
{-# INLINABLE updateF #-}
{-# INLINABLE queryF #-}

instance NFData a => NFData (FTree a) where
    rnf (FTree lrp rt) = rnf lrp `seq` rnf rt

instance NFData a => NFData (FNode a) where
    rnf FTip = ()
    rnf (FBin x l r) = rnf x `seq` rnf l `seq` rnf r
