{-|
Miscellaneous stuff

pairs
Generates distinct pairs from a list

fArray
Generates an Array from bounds and a function

chunksOf
Split a list into chunks of fixed size.
-}
module Misc where

import Data.Array
import Data.List

pairs :: [a] -> [(a, a)]
pairs a = [(x, y) | (x:ys) <- tails a, y <- ys]

fArray :: Ix i => (i, i) -> (i -> a) -> Array i a
fArray b f = array b [(i, f i) | i <- range b]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = go where
    go [] = []
    go xs = xs' : go xs'' where (xs', xs'') = splitAt n xs
