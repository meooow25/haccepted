{-# LANGUAGE FlexibleContexts, QuantifiedConstraints, ScopedTypeVariables #-}
{-|
Sorting

Data.List.sort is rather inefficient when we don't care about laziness and just want to fully sort a
list. An in-place sort can have much better performance. Benchmarks show that for a list of Ints,
sort and sortU are 4x and 8x faster than Data.List.sort.

sort, sortBy, sortU, sortUBy use merge sort. countingSortUA uses counting sort. Both are stable
sorts.

Sources:
* https://en.wikipedia.org/wiki/Merge_sort
* https://en.wikipedia.org/wiki/Counting_sort

sort
Sorts a list. O(n log n).

sortBy
Sorts a list with a comparison function. O(n log n).

sortU
Sorts a list for an element type that can be put in unboxed arrays. Faster than sort. O(n log n).

sortUBy
Sorts a list for an element type that can be put in unboxed arrays with a comparison function.
Faster than sortBy. O(n log n).

sortUABy
Sorts an unboxed array with a comparison function. O(n log n).

countingSortUA
Sorts an unboxed array using counting sort. f should be a function that maps every element to an Int
in [0..b-1]. O(n + b).
-}

module Sort
    ( sort
    , sortBy
    , sortU
    , sortUBy
    , sortUABy
    , countingSortUA
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.Base
import Data.Array.ST

import Misc ( modifyArray )

sort :: Ord e => [e] -> [e]
sort = sortBy compare

sortBy :: (e -> e -> Ordering) -> [e] -> [e]
sortBy cmp xs = elems $ runSTArray $ do
    a <- listArrayST (1, length xs) xs
    mergeSort a cmp
    pure a

sortU :: (Ord e, forall s. MArray (STUArray s) e (ST s), IArray UArray e) => [e] -> [e]
sortU = sortUBy compare

sortUBy :: (forall s. MArray (STUArray s) e (ST s), IArray UArray e)
        => (e -> e -> Ordering) -> [e] -> [e]
sortUBy cmp xs = elems $ runSTUArray $ do
    a <- listUArrayST (1, length xs) xs
    mergeSort a cmp
    pure a

sortUABy :: (forall s. MArray (STUArray s) e (ST s), IArray UArray e)
         => (e -> e -> Ordering) -> UArray Int e -> UArray Int e
sortUABy cmp a = runSTUArray $ do
    a' <- thaw a
    mergeSort a' cmp
    pure a'

mergeSort :: forall a e m. (MArray a e m) => a Int e -> (e -> e -> Ordering) -> m ()
mergeSort a cmp = do
    n <- getNumElements a
    b :: a Int e <- newArray_ (1, n)
    let merge l m r = foldM_ f (l, m) [l .. r-1] where
            f (i, j) k
                | i >= m = takej
                | j >= r = takei
                | otherwise = do
                    o <- cmp <$> unsafeRead a i <*> unsafeRead a j
                    if o /= GT then takei else takej
              where
                takei = (i + 1, j) <$ (unsafeWrite b k =<< unsafeRead a i)
                takej = (i, j + 1) <$ (unsafeWrite b k =<< unsafeRead a j)
    forM_ (takeWhile (<n) $ iterate (*2) 1) $ \w -> do
        forM_ [0, 2*w .. n-1] $ \i -> merge i ((i + w) `min` n) ((i + 2*w) `min` n)
        forM_ [0 .. n-1] $ \i -> unsafeRead b i >>= unsafeWrite a i
{-# INLINE mergeSort #-}

countingSortUA :: (IArray UArray e, forall s. MArray (STUArray s) e (ST s))
               => Int -> (e -> Int) -> UArray Int e -> UArray Int e
countingSortUA b f a = runSTUArray $ do
    cnt <- newArray (0, b) 0 :: ST s (STUArray s Int Int)
    forM_ (elems a) $ \x -> modifyArray cnt (f x + 1) (+1)
    writeArray cnt 0 (fst (bounds a))
    forM_ [1 .. b-1] $ \i -> readArray cnt (i - 1) >>= modifyArray cnt i . (+)
    a' <- newArray_ (bounds a)
    forM_ (elems a) $ \x -> do
        let y = f x
        i <- readArray cnt y
        writeArray cnt y (i + 1)
        writeArray a' i x
    pure a'

--------------------------------------------------------------------------------
-- For tests

-- Allows specialization across modules
{-# INLINABLE sortU #-}
{-# INLINABLE sortUBy #-}
{-# INLINABLE sortUABy #-}
{-# INLINABLE countingSortUA #-}
