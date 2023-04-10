{-# LANGUAGE DefaultSignatures, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Unboxed array support for types isomorphic to other types with unboxed array support.

As easy as "instance Unbox (Sum Int) Int".

Implementation notes:
* The problem: Unboxed array types (UArray, STUArray, IOUArray) have the element type role as
  nominal (https://gitlab.haskell.org/ghc/ghc/-/issues/9220). This makes it impossible to newtype
  derive IArray and MArray instances. And it's a PITA to write them manually for every newtype.
  No one wants to write 40 lines of instances each for Sum Int, Max Int, Mod 1000000007 Int.
* The solution: Use an unholy trinity of unsafeCoerce, overlappable instances, and an Iso typeclass
  (just like Vector.Unboxed.IsoUnbox). This still requires ~30 lines of code, but only once. The
  typeclass also makes it useful for more than just newtypes. If there is a better way, I would love
  to hear of it.
-}

module Unbox
    ( Unbox(..)
    ) where

import Data.Array.Base
import Data.Coerce
import Unsafe.Coerce

class Unbox a b | a -> b where
    toU :: a -> b
    default toU :: Coercible a b => a -> b
    toU = coerce
    frU :: b -> a
    default frU :: Coercible b a => b -> a
    frU = coerce

coerceToU :: Unbox a b => arr i a -> arr i b
coerceFrU :: Unbox a b => arr i b -> arr i a
coerceToU = unsafeCoerce
coerceFrU = unsafeCoerce

instance {-# OVERLAPPABLE #-} (Unbox a b, IArray arr b) => IArray arr a where
    bounds                     = bounds . coerceToU
    numElements                = numElements . coerceToU
    unsafeArray b ixs          = coerceFrU (unsafeArray b (map (fmap toU) ixs))
    unsafeAt a i               = frU (unsafeAt (coerceToU a) i)
    unsafeReplace a ixs        = coerceFrU (unsafeReplace (coerceToU a) (map (fmap toU) ixs))
    unsafeAccum f a iys        = coerceFrU (unsafeAccum (\x y -> toU (f (frU x) y)) (coerceToU a) iys)
    unsafeAccumArray f x b iys = coerceFrU (unsafeAccumArray (\x y -> toU (f (frU x) y)) (toU x) b iys)

coerceToMU :: Unbox a b => marr i a -> marr i b
coerceFrMU :: Unbox a b => marr i b -> marr i a
coerceToMU = unsafeCoerce
coerceFrMU = unsafeCoerce

instance {-# OVERLAPPABLE #-} (Unbox a b, Monad m, MArray marr b m) => MArray marr a m where
    getBounds         = getBounds . coerceToMU
    getNumElements    = getNumElements . coerceToMU
    unsafeNewArray_   = fmap coerceFrMU . unsafeNewArray_
    newArray_         = fmap coerceFrMU . newArray_
    unsafeRead a i    = fmap frU (unsafeRead (coerceToMU a) i)
    unsafeWrite a i x = unsafeWrite (coerceToMU a) i (toU x)
