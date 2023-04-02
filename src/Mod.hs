{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleInstances, KindSignatures, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-|
Modular arithmetic

Mod m i is an integer of type i modulo m.
m will usually be a compile-time constant. If m is not known at compile time, Mod can still be
used via GHC.TypeNats.SomeNat.
m must be >= 2 and fit in the type i.
Integer operations on i for values in [0..m-1] must not overflow. Examples to avoid are using (-)
with a word type or using (*) with a large enough mod that (m-1)^2 oveflows.

This is a very general type, for something simpler see MInt.hs.

Implementation notes:
* See note about unboxed array support in MInt.hs.

Instances of Eq, Num, Fractional exist for Mod m i. All the usual operations take O(1) time, except
for recip which takes O(log n) time. This assumes Integral i methods take O(1) time.
An instance of Enum exists for MInt. The enum is cyclic, it wraps to 0 after m-1.
An instance of IArray UArray MInt exists. Instances of MArray (STUArray s) MInt (ST s) and
MArray IOUArray MInt IO are also defined.

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

-- Imports for unboxed array support
import Data.Array.Base
import Data.Array.IO
import Data.Coerce
import Control.Monad.ST
import Unsafe.Coerce

import Math ( egcd )

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

--------------------------------------------------------------------------------
-- Unboxed array support

coerceUIM :: UArray i e -> UArray i (Mod m e)
coerceUIM = unsafeCoerce

coerceUMI :: UArray i (Mod m e) -> UArray i e
coerceUMI = unsafeCoerce

instance IArray UArray e => IArray UArray (Mod m e) where
    bounds                = bounds . coerceUMI
    numElements           = numElements . coerceUMI
    unsafeArray lu ies    = coerceUIM $ unsafeArray lu (coerce ies)
    unsafeAt arr i        = coerce $ unsafeAt (coerceUMI arr) i
    unsafeReplace arr ies = coerceUIM $ unsafeReplace (coerceUMI arr) (coerce ies)
    unsafeAccum f arr ies = coerceUIM $ unsafeAccum (coerce f) (coerceUMI arr) ies
    unsafeAccumArray f initialValue lu ies
                          = coerceUIM $ unsafeAccumArray (coerce f) (coerce initialValue) lu ies

coerceSTIM :: STUArray s i e -> STUArray s i (Mod m e)
coerceSTIM = unsafeCoerce

coerceSTMI :: STUArray s i (Mod m e) -> STUArray s i e
coerceSTMI = unsafeCoerce

instance MArray (STUArray s) e (ST s) => MArray (STUArray s) (Mod m e) (ST s) where
    getBounds           = getBounds . coerceSTMI
    getNumElements      = getNumElements . coerceSTMI
    unsafeNewArray_     = fmap coerceSTIM . unsafeNewArray_
    newArray_           = fmap coerceSTIM . newArray_
    unsafeRead arr i    = coerce <$> unsafeRead (coerceSTMI arr) i
    unsafeWrite arr i e = unsafeWrite (coerceSTMI arr) i (coerce e)

coerceIOIM :: IOUArray i e -> IOUArray i (Mod m e)
coerceIOIM = unsafeCoerce

coerceIOMI :: IOUArray i (Mod m e) -> IOUArray i e
coerceIOMI = unsafeCoerce

instance MArray IOUArray e IO => MArray IOUArray (Mod m e) IO where
    getBounds                = getBounds . coerceIOMI
    getNumElements           = getNumElements . coerceIOMI
    newArray lu initialValue = coerceIOIM <$> newArray lu (coerce initialValue)
    unsafeNewArray_          = fmap coerceIOIM . unsafeNewArray_
    newArray_                = fmap coerceIOIM . newArray_
    unsafeRead arr i         = coerce <$> unsafeRead (coerceIOMI arr) i
    unsafeWrite arr i e      = unsafeWrite (coerceIOMI arr) i (coerce e)

--------------------------------------------------------------------------------
-- For tests

instance NFData i => NFData (Mod m i) where
    rnf = rnf . unMod
