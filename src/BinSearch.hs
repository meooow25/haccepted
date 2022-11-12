{-|
Binary search

Source: https://en.wikipedia.org/wiki/Binary_search_algorithm#Procedure_for_finding_the_leftmost_element

Implementation note:
binSearch can be written in terms of binSearchM but is more much likely to be used so it is
separate for ease of copying.

The complexities below assume the predicate takes O(1) time, they should be scaled appropriately if
not so.

binSearch
Returns the first value in [l..h] which satisfies the predicate f, h + 1 otherwise.
Returns l if l > h. l + h should not overflow. O(log (h - l + 1)).

binSearchA
binSearch on an Array. O(log n).

binSearchM
binSearch but the predicate returns a monad. O(log (h - l + 1)).
-}

module BinSearch
    ( binSearch
    , binSearchA
    , binSearchM
    ) where

import Data.Array

binSearch :: Integral i => (i -> Bool) -> i -> i -> i
binSearch f = go where
    go l h
        | l > h     = l
        | f m       = go l (m - 1)
        | otherwise = go (m + 1) h
        where m = (l + h) `div` 2

binSearchA :: (a -> Bool) -> Array Int a -> Int
binSearchA f a = binSearch (f . (a!)) l h where (l, h) = bounds a

binSearchM :: (Monad m, Integral i) => (i -> m Bool) -> i -> i -> m i
binSearchM f = go where
    go l h | l > h = pure l
    go l h = do
        let m = (l + h) `div` 2
        v <- f m
        if v then go l (m - 1) else go (m + 1) h

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE binSearch #-}
{-# INLINABLE binSearchA #-}
