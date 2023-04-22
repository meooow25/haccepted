{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses,
             TypeFamilies, UndecidableInstances #-}
{-|
Arrays

Stuff to make it easier to work with arrays from the array package, especially when it comes to
unboxed arrays. If the vector package is available, just use that.

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
* Indexing is as fast as the underlying representation but construction via listArray and array are
  known to be slower. See ArrayBench.hs. TODO: Figure out why and fix it.
* unsafeAccum and unsafeAccumArray are optional but explicity error for Arr2 because the default
  definitions use STArray. This is horribly slow with unboxed arrays. Better error than TLE.
* TODO: Implement freeze and unsafeFreeze.

Arr
Array type for element types isomorphic to other element types with existing array support.
Primarily intended as a way to get unboxed arrays.
As an example, if you have "newtype N = N Int", define "instance Unbox N where type Unboxed N = Int"
and use UArr i N as an unboxed array for N. This is a lot more performant than a boxed Array i N.
Also works with mutable arrays, IOUArr and STUArr.

Arr2
Arrays for 2-tuples
Primarily intended as a way to get unboxed arrays.
If a and b can be put in UArrays, UArr2 i (a,b) works as an unboxed array for (a,b).
This is a lot more performant than a boxed Array i (a,b). Also works with mutable arrays, STUArr2
and IOUArr2.
Can be nested to get UArr3, UArr4, etc. Use with Unbox and Arr to store your own types.
-}

module Array
    ( Unbox(..)
    , Arr
    , UArr
    , IOUArr
    , STUArr
    , Arr2
    , UArr2
    , UArr3
    , UArr4
    , STUArr2
    , STUArr3
    , STUArr4
    , IOUArr2
    , IOUArr3
    , IOUArr4
    ) where

import Control.DeepSeq
import Data.Array.Base
import Data.Array.IO
import Data.Coerce
import Data.Semigroup

--------------------------------------------------------------------------------
-- Unbox and Arr

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

instance Unbox (Sum a) where
    type Unboxed (Sum a) = a

instance Unbox (Min a) where
    type Unboxed (Min a) = a

instance Unbox (Max a) where
    type Unboxed (Max a) = a

--------------------------------------------------------------------------------
-- Arr2

data Arr2 arra arrb i ab where
    Arr2 :: !(arra i a) -> !(arrb i b) -> Arr2 arra arrb i (a, b)

instance (IArray arra a, IArray arrb b) => IArray (Arr2 arra arrb) (a, b) where
    bounds (Arr2 xa _)              = bounds xa
    numElements (Arr2 xa _)         = numElements xa
    unsafeArray b ixys              = Arr2 (unsafeArray b (map (fmap fst) ixys)) (unsafeArray b (map (fmap snd) ixys))
    unsafeAt (Arr2 xa ya) i         = (unsafeAt xa i, unsafeAt ya i)
    unsafeReplace (Arr2 xa ya) ixys = Arr2 (unsafeReplace xa (map (fmap fst) ixys)) (unsafeReplace ya (map (fmap snd) ixys))
    unsafeAccum                     = error "Arr2 unsafeAccum: not implemented"
    unsafeAccumArray                = error "Arr2 unsafeAccumArray: not implemented"

instance (IArray (Arr2 arra arrb) ab, Ix i, Show i, Show ab) => Show (Arr2 arra arrb i ab) where
    showsPrec = showsIArray

instance (Monad m, MArray marra a m, MArray marrb b m) => MArray (Arr2 marra marrb) (a, b) m where
    getBounds (Arr2 xa _)             = getBounds xa
    getNumElements (Arr2 xa _)        = getNumElements xa
    unsafeNewArray_ b                 = Arr2 <$> unsafeNewArray_ b <*> unsafeNewArray_ b
    newArray_ b                       = Arr2 <$> newArray_ b <*> newArray_ b
    unsafeRead (Arr2 xa ya) i         = (,) <$> unsafeRead xa i <*> unsafeRead ya i
    unsafeWrite (Arr2 xa ya) i (x, y) = unsafeWrite xa i x *> unsafeWrite ya i y

type UArr2 = Arr2 UArray UArray
type UArr3 = Arr2 UArray UArr2
type UArr4 = Arr2 UArr2 UArr2

type STUArr2 s = Arr2 (STUArray s) (STUArray s)
type STUArr3 s = Arr2 (STUArray s) (STUArr2 s)
type STUArr4 s = Arr2 (STUArr2 s) (STUArr2 s)

type IOUArr2 = Arr2 IOUArray IOUArray
type IOUArr3 = Arr2 IOUArray IOUArr2
type IOUArr4 = Arr2 IOUArr2 IOUArr2

--------------------------------------------------------------------------------
-- For tests

instance NFData (arr i (Unboxed a)) => NFData (Arr arr i a) where
    rnf = rnf . unArr

instance (NFData (arra i a), NFData (arrb i b)) => NFData (Arr2 arra arrb i (a, b)) where
    rnf (Arr2 xa ya) = rnf xa `seq` rnf ya
