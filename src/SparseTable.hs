{-# LANGUAGE FlexibleContexts, QuantifiedConstraints #-}
{-|
Sparse table

Structure for fast static range fold queries. Useful when the elements do not form a group,
otherwise prefix sums can be used.

Sources:
* https://cp-algorithms.com/data_structures/sparse-table.html
* https://github.com/kth-competitive-programming/kactl/blob/main/content/data-structures/RMQ.h

Implementation notes:
* Some array elements are undefined (because of newArray_), but that is fine because those elements
  are never read.
* I would love to put Semigroup/Idempotent constraints on fromListU functions but deriving IArray
  and MArray instances for newtypes is an unfair amount of pain.

Let n = r - l + 1 in all instances below.

fromListSP
Constructs a range fold function from a list. O(n log n) to construct the structure and O(log n)
for each query.

fromListISP
Constructs a range fold function from a list, when the semigroup is idempotent. O(n log n) to
construct the structure and O(1) for each query.

fromListUSP
Constructs a range fold function from a list. Uses an unboxed array. O(n log n) to construct the
structure and O(log n) for each query.

fromListIUSP
Constructs a range fold function from a list, when the semigroup is idempotent. Uses an unboxed
array. O(n log n) to construct the structure and O(1) for each query.

buildSP
Builds a sparse table. O(n log n). Prefer the fromList functions.

foldSP
Folds a range on a sparse table. O(log n). Prefer the fromList functions.

foldISP
Folds a range on a sparse table, when the semigroup is idempotent. O(1). Prefer the fromList
functions.
-}

module SparseTable
    ( fromListSP
    , fromListISP
    , fromListUSP
    , fromListIUSP
    , buildSP
    , foldSP
    , foldISP
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits

import Misc ( Idempotent, bitLength )

fromListSP :: Semigroup e => (Int, Int) -> [e] -> Int -> Int -> e
fromListSP bnds xs = foldSP (<>) $! runSTArray $ buildSP (<>) bnds xs

fromListISP :: Idempotent e => (Int, Int) -> [e] -> Int -> Int -> e
fromListISP bnds xs = foldISP (<>) $! runSTArray $ buildSP (<>) bnds xs

fromListUSP :: (IArray UArray e, forall s. MArray (STUArray s) e (ST s))
            => (e -> e -> e) -> (Int, Int) -> [e] -> Int -> Int -> e
fromListUSP op bnds xs = foldSP op $! runSTUArray $ buildSP op bnds xs

fromListIUSP :: (IArray UArray e, forall s. MArray (STUArray s) e (ST s))
             => (e -> e -> e) -> (Int, Int) -> [e] -> Int -> Int -> e
fromListIUSP op bnds xs = foldISP op $! runSTUArray $ buildSP op bnds xs

buildSP :: MArray a e (ST s) => (e -> e -> e) -> (Int, Int) -> [e] -> ST s (a (Int, Int) e)
buildSP _  (l, r) _ | l > r = error "buildSP: empty range"
buildSP op (l, r) xs = do
    let h = bitLength (r - l + 1) - 1
    t <- newArray_ ((0, l), (h, r))
    forM_ (zip [l..r] xs) $ \(i, x) -> writeArray t (0, i) x
    forM_ [1..h] $ \j -> do
        let d = bit (j - 1)
        forM_ [l..r-2*d+1] $ \i ->
            op <$> readArray t (j-1, i) <*> readArray t (j-1, i+d) >>= (writeArray t (j, i) $!)
    pure t
{-# INLINE buildSP #-}

foldSP :: IArray a e => (e -> e -> e) -> a (Int, Int) e -> Int -> Int -> e
foldSP op t = qry where
    qry l r | l > r = error "foldSP: empty range"
    qry l r = go (l + bit j) (t!(j, l)) where
        j = countTrailingZeros (r - l + 1)
        go l' acc | l' > r = acc
        go l' acc = go (l' + bit j') $! op acc (t!(j', l')) where
            j' = countTrailingZeros (r - l' + 1)
{-# INLINE foldSP #-}

foldISP :: IArray a e => (e -> e -> e) -> a (Int, Int) e -> Int -> Int -> e
foldISP op t = qry where
    qry l r | l > r = error "foldISP: empty range"
    qry l r = op (t!(j, l)) (t!(j, r + 1 - bit j)) where
        j = bitLength (r - l + 1) - 1
{-# INLINE foldISP #-}

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE fromListSP #-}
{-# INLINABLE fromListISP #-}
{-# INLINABLE fromListUSP #-}
{-# INLINABLE fromListIUSP #-}
