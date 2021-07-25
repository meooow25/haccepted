module BinSearch where

import Data.Array

-- Standard binary search
-- Gets the first value in [l, h) which satisfies the predicate f, h otherwise
-- Source: https://en.wikipedia.org/wiki/Binary_search_algorithm#Procedure_for_finding_the_leftmost_element
-- Complexity: O (log (h - l))

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