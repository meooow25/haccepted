{-|
Miscellaneous functions/recipes

pairs
Generates distinct pairs of elements from a list. O(n^2).

fArray
Generates an Array from bounds and a function. O(n) assuming f takes O(1).

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
O(n log n) assuming f takes O(1).

modifyArray
Modifies an element in a mutable array.

modifyArray'
Modifies an element in a mutable array. Strict version.

foldMComp
Compose a strict left fold function with a mapM function. Useful for foldMs.
foldM (f `foldMComp` g) z = fmap (foldl' f z) . mapM g

farthest
Repeatedly applies a function to a value and returns the value which gives back Nothing.

foldTree'
Folds a tree. This does the same job as Data.Tree.foldTree but with different fold functions, which
may be preferable in cases such as CPS folds.
-}

module Misc
    ( pairs
    , fArray
    , chunksOf
    , replicateL
    , unique
    , foldExclusive
    , modifyArray
    , modifyArray'
    , foldMComp
    , farthest
    , foldTree'
    ) where

import Control.Monad
import Data.Array.IArray
import Data.Array.MArray
import Data.List
import Data.Tree

pairs :: [a] -> [(a, a)]
pairs xs = [(x, x') | (x:xs') <- tails xs, x' <- xs']

fArray :: (IArray a e, Ix i) => (i, i) -> (i -> e) -> a i e
fArray b f = listArray b (f <$> range b)
{-# INLINE fArray #-}

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr f where
    f [] = Nothing
    f xs = Just (splitAt n xs)

replicateL :: Int -> [a] -> [a]
replicateL n = concat . replicate n

unique :: Eq a => [a] -> [a]
unique = map head . group

foldExclusive :: (b -> a -> b) -> b -> [a] -> [b]
foldExclusive _ _ [] = []
foldExclusive f b as = go b (length as) as [] where
    go b 1 _  = (b:)
    go b n as = b1 `seq` b2 `seq` go b2 n' as1 . go b1 (n - n') as2 where
        n' = n `div` 2
        (as1, as2) = splitAt n' as
        b1 = foldl' f b as1
        b2 = foldl' f b as2

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f = readArray a i >>= writeArray a i . f
{-# INLINE modifyArray #-}

modifyArray' :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray' a i f = do
    x <- readArray a i
    let x' = f x
    x' `seq` writeArray a i x'
{-# INLINE modifyArray' #-}

foldMComp :: Monad m => (b -> a -> b) -> (c -> m a) -> b -> c -> m b
foldMComp f g = \z x -> f z <$!> g x
{-# INLINE foldMComp #-}

farthest :: (a -> Maybe a) -> a -> a
farthest f = go where go x = maybe x go (f x)

foldTree' :: (a -> b -> c) -> (c -> b -> b) -> b -> Tree a -> c
foldTree' f g z = go where go (Node x ts) = f x (foldr (g . go) z ts)
{-# INLINE foldTree' #-}
