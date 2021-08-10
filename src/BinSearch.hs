{-|
Standard binary search

binSearch
Returns the first value in [l, h) which satisfies the predicate f, h otherwise. Complexity is
O(log (h - l)) times that of f.

* Returns l if l > h
* l + h should not overflow

Source: https://en.wikipedia.org/wiki/Binary_search_algorithm#Procedure_for_finding_the_leftmost_element

binSearchA
binSearch on an Array

binSearchM
binSearch but the predicate returns a monad
-}

module BinSearch where

import Data.Array

binSearch :: (Integral i) => (i -> Bool) -> i -> i -> i
binSearch f l h
    | l >= h    = l
    | f m       = binSearch f l m
    | otherwise = binSearch f (m + 1) h
    where m = (l + h) `div` 2

binSearchA :: (a -> Bool) -> Array Int a -> Int
binSearchA f a = binSearch (f . (a!)) l (h + 1) where (l, h) = bounds a

binSearchM :: (Monad m, Integral i) => (i -> m Bool) -> i -> i -> m i
binSearchM f l h
    | l >= h    = return l
    | otherwise = do
        v <- f m
        if v then binSearchM f l m else binSearchM f (m + 1) h
    where m = (l + h) `div` 2
