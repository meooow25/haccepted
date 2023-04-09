{-# LANGUAGE AllowAmbiguousTypes, BangPatterns, ScopedTypeVariables #-}
{-|
Math

Sources:
* https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm

egcd
Returns (g, s) where g = gcd(a, b); as + bt = g. O(log min(a, b)). Also see note for egcd2.

egcd2
Returns (g, s, t) where g = gcd(a, b); as + bt = g. O(log min(a, b)).
Note: If the inputs are negative the returned gcd may be negative. abs the inputs or sign flip the
outputs if this is undesirable. The complexity assumes operations quotRem, (-), (*), (==0) all
take O(1).

mkFactorials
Calculate factorials for 0..n. O(n).

mkInvFactorials
Given 1/(n!) calculcate inverse factorials for 0..n. O(n).

mkBinom
Given the maximum value of n, calculate binomial coefficients for n, k. Apply partially for multiple
queries. O(maxn) to set up, O(1) per query.
-}

module Math
    ( egcd
    , egcd2
    , mkInvFactorials
    , mkFactorials
    , mkBinom
    ) where

import Data.Array.Unboxed
import Data.List

egcd :: Integral i => i -> i -> (i, i)
egcd = go 1 0 where
    go !s !_ !r 0  = (r, s)
    go s  s' r  r' = let (q, r'') = quotRem r r' in go s' (s - q * s') r' r''
{-# INLINE egcd #-}

egcd2 :: Integral i => i -> i -> (i, i, i)
egcd2 = go 1 0 0 1 where
    go !s !_ !t !_ !r 0  = (r, s, t)
    go s  s' t  t' r  r' = let (q, r'') = quotRem r r' in go s' (s - q * s') t' (t - q * t') r' r''
{-# INLINE egcd2 #-}

mkFactorials :: (IArray a e, Num e) => Int -> a Int e
mkFactorials n = listArray (0,n) $ scanl' (*) 1 $ map fromIntegral [1..n]

mkInvFactorials :: (IArray a e, Num e) => Int -> e -> a Int e
mkInvFactorials n invfacn =
    array (0,n) $ zip [n, n-1 .. 0] $ scanl' (*) invfacn $ map fromIntegral [n, n-1 .. 1]

mkBinom :: forall a e. (IArray a e, Fractional e) => Int -> Int -> Int -> e
mkBinom mxn = mkBinom_ fac ifac where
    fac, ifac :: a Int e
    fac = mkFactorials mxn
    ifac = mkInvFactorials mxn (recip (fac!mxn))

mkBinom_ :: (IArray a e, Num e) => a Int e -> a Int e -> Int -> Int -> e
mkBinom_ !fac !ifac = go where
    go n k | k < 0 || n < k = 0
    go n k = fac!n * ifac!k * ifac!(n - k)

--------------------------------------------------------------------------------
-- For tests

{-# INLINABLE mkFactorials #-}
{-# INLINABLE mkInvFactorials #-}
