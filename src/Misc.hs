{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
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
Folds strictly such that the ith element of the output list contains the fold of all elements in the
input list except for the ith element. The fold function f must be commutative, in the sense that
(b `f` a1) `f` a2 = (b `f` a2) `f` a1
f is called O(n log n) times.

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

Commutative
A semigroup where the operation (<>) is commutative.
a <> b = b <> a

Group
A monoid where elements have inverses.
a <> invert a = mempty
invert a <> a = mempty

Idempotent
A semigroup where the elements are idempotent.
a <> a = a

Action
A right monoid action of u on a.
(x `act` u1) `act` u2 = x `act` (u1 <> u2)
x `act` mempty = x

bitLength
The number of bits required to represent the value.

unsafeBit
Just like bit, but skips the check that the index is in [0 .. size-1].

odds
The elements at odd positions of a list.

evens
The elements at even positions of a list.

orM
Short-circuiting monadic ||.

andM
Short-circuiting monadic &&.

anyM
Monadic version of any.

allM
Monadic version of all.
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
    , Commutative
    , Group(..)
    , Idempotent
    , Action(..)
    , bitLength
    , unsafeBit
    , odds
    , evens
    , orM
    , andM
    , anyM
    , allM
    ) where

import Control.Monad
import Data.Array.IArray
import Data.Array.MArray
import Data.Bits
import Data.List
import Data.Semigroup
import Data.Tree

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | x:ys <- tails xs, y <- ys]

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
foldExclusive f y0 xs0 = go y0 (length xs0) xs0 [] where
    go !y 1 _ = (y:)
    go y n xs = go yr n' xsl . go yl (n - n') xsr where
        n' = n `div` 2
        (xsl, xsr) = splitAt n' xs
        yl = foldl' f y xsl
        yr = foldl' f y xsr

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f = readArray a i >>= writeArray a i . f
{-# INLINE modifyArray #-}

modifyArray' :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray' a i f = readArray a i >>= (writeArray a i $!) . f
{-# INLINE modifyArray' #-}

foldMComp :: Monad m => (b -> a -> b) -> (c -> m a) -> b -> c -> m b
foldMComp f g = \z x -> f z <$!> g x
{-# INLINE foldMComp #-}

farthest :: (a -> Maybe a) -> a -> a
farthest f = go where go x = maybe x go (f x)

foldTree' :: (a -> b -> c) -> (c -> b -> b) -> b -> Tree a -> c
foldTree' f g z = go where go (Node x ts) = f x (foldr (g . go) z ts)
{-# INLINE foldTree' #-}

class Semigroup a => Commutative a

class Monoid a => Group a where
    invert :: a -> a

instance Num a => Commutative (Sum a)

instance Num a => Group (Sum a) where
    invert = negate
    {-# INLINE invert #-}

class Semigroup a => Idempotent a

instance Ord a => Idempotent (Max a)

instance Ord a => Idempotent (Min a)

instance Idempotent (First a)

instance Idempotent (Last a)

class (Monoid u, Monoid a) => Action u a where
    act :: a -> u -> a

bitLength :: FiniteBits b => b -> Int
bitLength x = finiteBitSize x - countLeadingZeros x
{-# INLINE bitLength #-}

unsafeBit :: (Bits a, Num a) => Int -> a
unsafeBit = unsafeShiftL 1
{-# INLINE unsafeBit #-}

odds :: [a] -> [a]
odds (_:x:xs) = x : odds xs
odds _        = []

evens :: [a] -> [a]
evens (x:_:xs) = x : evens xs
evens [x]      = [x]
evens []       = []

orM :: Monad m => m Bool -> m Bool -> m Bool
orM m1 m2 = m1 >>= \x -> if x then pure True else m2

andM :: Monad m => m Bool -> m Bool -> m Bool
andM m1 m2 = m1 >>= \x -> if x then m2 else pure False

anyM :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m Bool
anyM f = foldr (orM . f) (pure False)

allM :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m Bool
allM f = foldr (andM . f) (pure True)
