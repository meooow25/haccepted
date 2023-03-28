{-# LANGUAGE BangPatterns #-}
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
-}

module Math
    ( egcd
    , egcd2
    ) where

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
