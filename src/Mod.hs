{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleInstances, KindSignatures, MultiParamTypeClasses,
             ScopedTypeVariables #-}
{-|
Modular arithmetic

Mod m i is an integer of type i modulo m.
m will usually be a compile-time constant. If m is not known at compile time, Mod can still be
used via GHC.TypeNats.SomeNat.
m must be >= 2 and fit in the type i.
Integer operations on i for values in [0..m-1] must not overflow. Examples to avoid are using (-)
with a word type or using (*) with a large enough mod that (m-1)^2 oveflows.

This is a very general type, for something simpler see MInt.hs.

Instances of Eq, Num, Fractional exist for Mod m i. All the usual operations take O(1) time, except
for recip which takes O(log n) time. This assumes Integral i methods take O(1) time.
An instance of Enum exists for MInt. The enum is cyclic, it wraps to 0 after m-1.
Unboxed array support is available via Unbox.

Mod
Type for modular arithmetic modulo m with underlying type i.

M7
Commonly used modulus.

M3
Commonly used modulus.

invMaybe
The multiplicative inverse modulo m. It exists if and only if the number is coprime to m. O(log n).
-}

module Mod
    ( Mod(..)
    , M7
    , M3
    , invMaybe
    ) where

import Control.DeepSeq
import Data.Maybe
import Data.Proxy
import Data.Ratio
import GHC.TypeNats ( KnownNat, Nat, natVal )

import Math ( egcd )
import Unbox ( Unbox )

newtype Mod (m :: Nat) i = Mod { unMod :: i } deriving (Eq, Ord, Show)

type M7 = Mod 1000000007 Int

type M3 = Mod 998244353 Int

instance (KnownNat m, Integral i) => Num (Mod m i) where
    Mod a + Mod b = Mod $ if c >= m then c - m else c where
        c = a + b
        m = fromIntegral (natVal (Proxy :: Proxy m))
    Mod a - Mod b = Mod $ if c < 0 then c + m else c where
        c = a - b
        m = fromIntegral (natVal (Proxy :: Proxy m))
    Mod a * Mod b = Mod $ a * b `mod` fromIntegral (natVal (Proxy :: Proxy m))
    abs         = id
    signum      = Mod . signum . unMod
    fromInteger = Mod . fromInteger . (`mod` fromIntegral (natVal (Proxy :: Proxy m)))

invMaybe :: forall m i. (KnownNat m, Integral i) => Mod m i -> Maybe (Mod m i)
invMaybe n = case egcd (unMod n) m of
    (1, s) | s >= 0    -> Just (Mod s)
           | otherwise -> Just (Mod (s + m))
    _ -> Nothing
  where
    m = fromIntegral (natVal (Proxy :: Proxy m))

instance (KnownNat m, Integral i) => Fractional (Mod m i) where
    recip          = fromMaybe (error "recip: no inverse") . invMaybe
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance (KnownNat m, Integral i) => Enum (Mod m i) where
    toEnum                 = fromIntegral
    fromEnum               = fromIntegral . unMod
    enumFromTo x y         = takeWhile (/= y + 1) [x..]
    enumFromThenTo x1 x2 y = takeWhile (/= y + 1) [x1, x2 ..]

instance Unbox (Mod m i) i

--------------------------------------------------------------------------------
-- For tests

instance NFData i => NFData (Mod m i) where
    rnf = rnf . unMod
