{-|
Miscellaneous functions/recipes

pairs
Generates distinct pairs of elements from a list. O(n^2).

fArray
Generates an Array from bounds and a function. O(n) assuming f runs in O(1).

chunksOf
Splits a list into chunks of fixed size. O(n).

replicateL
Replicates a list n times. O(nm) where m is the length of the list.

unique
Eliminates consecutive duplicate elements. O(n).

foldExclusive
Folds a list of values such that the ith element of the result contains the folded result of all
elements in the input list excluding the ith element. The fold is strict. The elements get folded
in a not-very-simple order, so the following should hold:
(b `f` a1) `f` a2 = (b `f` a2) `f` a1
O(n log n).
-}

module Misc
    ( pairs
    , fArray
    , chunksOf
    , replicateL
    , unique
    , foldExclusive
    ) where

import Data.Array
import Data.List

pairs :: [a] -> [(a, a)]
pairs xs = [(x, x') | (x:xs') <- tails xs, x' <- xs']

fArray :: Ix i => (i, i) -> (i -> a) -> Array i a
fArray b f = array b [(i, f i) | i <- range b]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = go where
    go [] = []
    go xs = xs' : go xs'' where (xs', xs'') = splitAt n xs

replicateL :: Int -> [a] -> [a]
replicateL n = concat . replicate n

unique :: Eq a => [a] -> [a]
unique = map head . group

foldExclusive :: (b -> a -> b) -> b -> [a] -> [b]
foldExclusive _ _ [] = []
foldExclusive f b as = go b (length as) as [] where
    go b 1 _  acc = b:acc
    go b n as acc = b1 `seq` b2 `seq` go b2 n' as1 $ go b1 (n - n') as2 acc where
        n' = n `div` 2
        (as1, as2) = splitAt n' as
        b1 = foldl' f b as1
        b2 = foldl' f b as2
