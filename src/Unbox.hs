{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             TypeFamilies, UndecidableInstances #-}
{-|
Arrays

Array type for element types isomorphic to other element types with existing array support.

Primarily useful for unboxed arrays.
As an example, define "instance Unbox (Sum Int) where type Unboxed (Sum Int) = Int" and use
UArr i (Sum Int) as an unboxed array for Sum Int.

Implementation notes:
* The problem: Unboxed array types (UArray, STUArray, IOUArray) have the element type role as
  nominal (https://gitlab.haskell.org/ghc/ghc/-/issues/9220). This makes it impossible to newtype
  derive IArray and MArray instances. And it's a PITA to write them manually for every newtype.
  No one wants to write 40 lines of instances each for Sum Int, Max Int, Mod 1000000007 Int.
* The solution: Use a newtype wrapper and an iso class (a bit like Vector.Unboxed.IsoUnbox). This
  still requires ~30 lines of code, but only once. The typeclass also makes it useful for more than
  just newtypes. This is the cleanest approach I got so far. If there is a better way, I would love
  to hear of it.
* We use an associated type over fundeps (class Unbox a b | a -> b) because Arr would then need to
  be Arr b arr i a and you would have to specify b when using it even though a determines b.
-}

module Unbox
    ( Unbox(..)
    , Arr
    , UArr
    , IOUArr
    , STUArr
    ) where

import Data.Array.Base
import Data.Array.IO
import Data.Coerce

class Unbox a where
    type Unboxed a
    toU :: a -> Unboxed a
    default toU :: Coercible a (Unboxed a) => a -> Unboxed a
    toU = coerce
    frU :: Unboxed a -> a
    default frU :: Coercible (Unboxed a) a => Unboxed a -> a
    frU = coerce

newtype Arr arr i a = Arr { unArr :: arr i (Unboxed a) }

instance (Unbox a, IArray arr (Unboxed a)) => IArray (Arr arr) a where
    bounds                     = bounds . unArr
    numElements                = numElements . unArr
    unsafeArray b ixs          = Arr (unsafeArray b (map (fmap toU) ixs))
    unsafeAt a i               = frU (unsafeAt (unArr a) i)
    unsafeReplace a ixs        = Arr (unsafeReplace (unArr a) (map (fmap toU) ixs))
    unsafeAccum f a iys        = Arr (unsafeAccum (\x y -> toU (f (frU x) y)) (unArr a) iys)
    unsafeAccumArray f x b iys = Arr (unsafeAccumArray (\x y -> toU (f (frU x) y)) (toU x) b iys)

instance (IArray (Arr arr) a, Ix i, Show i, Show a) => Show (Arr arr i a) where
    showsPrec = showsIArray

instance (Unbox a, Monad m, MArray marr (Unboxed a) m) => MArray (Arr marr) a m where
    getBounds         = getBounds . unArr
    getNumElements    = getNumElements . unArr
    unsafeNewArray_   = fmap Arr . unsafeNewArray_
    newArray_         = fmap Arr . newArray_
    unsafeRead a i    = fmap frU (unsafeRead (unArr a) i)
    unsafeWrite a i x = unsafeWrite (unArr a) i (toU x)

type UArr = Arr UArray
type IOUArr = Arr IOUArray
type STUArr s = Arr (STUArray s)
