{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Modular arithmetic

MInt is a newtype of Int for arithmetic modulo a known fixed prime.
For a more general type see Mod.hs.

Implementation notes:
* MInt is a newtype of Int, change to Int64 if running on 32-bit.
* Regarding unsafeCoerce (trust me it's safe!), type roles for unboxed arrays are nominal (and not
  representational), so trying to derive unboxed arrays for MInt does not work (see
  https://gitlab.haskell.org/ghc/ghc/-/issues/9220). As I see it my options are
    a. spill more guts out of Array.Base and Array.IO to copy-paste everything an Int array does
    b. use unsafeCoerce
  I would love to hear of a better way.

Instances of Eq, Num, Fractional exist for MInt. All the usual operations take O(1) time, except for
recip which takes O(log m) time.
An instance of Enum exists for MInt. The enum is cyclic, it wraps to 0 after m-1.
An instance of IArray UArray MInt exists. Instances of MArray (STUArray s) MInt (ST s) and
MArray IOUArray MInt IO are also defined.

MInt
Int type for arithmetic modulo a fixed prime mm.

mm
The prime modulus.
-}

module MInt
    ( MInt(..)
    , mm
    ) where

import Control.DeepSeq
import Data.Ratio

-- Imports for unboxed array support
import Control.Monad.ST
import Data.Array.Base
import Data.Array.IO
import Data.Coerce
import Unsafe.Coerce

mm :: Int
mm = 1000000007
-- m = 998244353

newtype MInt = MInt { unMInt :: Int } deriving (Eq, Ord, Show)

instance Num MInt where
    MInt a + MInt b = MInt $ let c = a + b in if c >= mm then c - mm else c
    MInt a - MInt b = MInt $ let c = a - b in if c < 0 then c + mm else c
    MInt a * MInt b = MInt $ a * b `mod` mm
    abs             = id
    signum          = MInt . signum . unMInt
    fromInteger     = MInt . fromInteger . (`mod` fromIntegral mm)

instance Fractional MInt where
    recip          = (^(mm - 2))
    fromRational r = fromIntegral (numerator r) / fromIntegral (denominator r)

instance Enum MInt where
    toEnum                 = fromIntegral
    fromEnum               = unMInt
    enumFromTo x y         = takeWhile (/= y + 1) [x..]
    enumFromThenTo x1 x2 y = takeWhile (/= y + 1) [x1, x2 ..]

--------------------------------------------------------------------------------
-- Unboxed array support

coerceUIM :: UArray i Int -> UArray i MInt
coerceUIM = unsafeCoerce

coerceUMI :: UArray i MInt -> UArray i Int
coerceUMI = unsafeCoerce

instance IArray UArray MInt where
    bounds                = bounds . coerceUMI
    numElements           = numElements . coerceUMI
    unsafeArray lu ies    = coerceUIM $ unsafeArray lu (coerce ies)
    unsafeAt arr i        = coerce $ unsafeAt (coerceUMI arr) i
    unsafeReplace arr ies = coerceUIM $ unsafeReplace (coerceUMI arr) (coerce ies)
    unsafeAccum f arr ies = coerceUIM $ unsafeAccum (coerce f) (coerceUMI arr) ies
    unsafeAccumArray f initialValue lu ies
                          = coerceUIM $ unsafeAccumArray (coerce f) (coerce initialValue) lu ies

coerceSTIM :: STUArray s i Int -> STUArray s i MInt
coerceSTIM = unsafeCoerce

coerceSTMI :: STUArray s i MInt -> STUArray s i Int
coerceSTMI = unsafeCoerce

instance MArray (STUArray s) MInt (ST s) where
    getBounds           = getBounds . coerceSTMI
    getNumElements      = getNumElements . coerceSTMI
    unsafeNewArray_     = fmap coerceSTIM . unsafeNewArray_
    newArray_           = fmap coerceSTIM . newArray_
    unsafeRead arr i    = coerce <$> unsafeRead (coerceSTMI arr) i
    unsafeWrite arr i e = unsafeWrite (coerceSTMI arr) i (coerce e)

coerceIOIM :: IOUArray i Int -> IOUArray i MInt
coerceIOIM = unsafeCoerce

coerceIOMI :: IOUArray i MInt -> IOUArray i Int
coerceIOMI = unsafeCoerce

instance MArray IOUArray MInt IO where
    getBounds                = getBounds . coerceIOMI
    getNumElements           = getNumElements . coerceIOMI
    newArray lu initialValue = coerceIOIM <$> newArray lu (coerce initialValue)
    unsafeNewArray_          = fmap coerceIOIM . unsafeNewArray_
    newArray_                = fmap coerceIOIM . newArray_
    unsafeRead arr i         = coerce <$> unsafeRead (coerceIOMI arr) i
    unsafeWrite arr i e      = unsafeWrite (coerceIOMI arr) i (coerce e)

--------------------------------------------------------------------------------
-- For tests

instance NFData MInt where
    rnf = rwhnf
