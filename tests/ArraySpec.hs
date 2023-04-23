{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             ScopedTypeVariables, TypeFamilies, TypeApplications #-}
module ArraySpec where

import Data.Array.IO
import Data.Array.Unboxed

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Array ( Arr2, IOUArr, UArr, Unbox(..) )

spec :: Spec
spec = do
    describe "Arr" $ do
        describe "IArray" $ do
            describe "listArray and elems" $ do
                prop "X Int" $ testListArrayElems @UArr @(X Int)
                prop "Maybe Word" $ testListArrayElems @UArr @(Maybe Word)
        describe "MArray" $ do
            describe "newListArray and getElems" $ do
                prop "X Int" $ testNewListArrayGetElems @IOUArr @(X Int)
                prop "Maybe Word" $ testNewListArrayGetElems @IOUArr @(Maybe Word)
            describe "writeArray and readArray" $ do
                prop "X Int" $ testReadWriteArray @IOUArr @(X Int)
                prop "Maybe Word" $ testReadWriteArray @IOUArr @(Maybe Word)
    describe "Arr2" $ do
        describe "IArray" $ do
            describe "listArray and elems" $ do
                prop "Array UArray (Int, Word)" $ testListArrayElems @(Arr2 Array UArray) @(Int, Word)
                prop "UArray UArray (Int, Word)" $ testListArrayElems @(Arr2 UArray UArray) @(Int, Word)
        describe "MArray" $ do
            describe "newListArray and getElems" $ do
                prop "IOArray IOUArray (Int, Word)" $ testNewListArrayGetElems @(Arr2 IOArray IOUArray) @(Int, Word)
                prop "IOUArray IOUArray (Int, Word)" $ testNewListArrayGetElems @(Arr2 IOUArray IOUArray) @(Int, Word)
            describe "writeArray and readArray" $ do
                prop "IOArray IOUArray (Int, Word)" $ testReadWriteArray @(Arr2 IOArray IOUArray) @(Int, Word)
                prop "IOUArray IOUArray (Int, Word)" $ testReadWriteArray @(Arr2 IOUArray IOUArray) @(Int, Word)

newtype X a = X a deriving (Eq, Ord, Show, Arbitrary)

instance Unbox (X a) where
    type Unboxed (X a) = a

-- Assume Just maxBound is never tested
instance Unbox (Maybe Word) where
    type Unboxed (Maybe Word) = Word
    toU Nothing = maxBound
    toU (Just x) = x
    frU x | x == maxBound = Nothing
          | otherwise     = Just x

testListArrayElems
    :: forall arr a. (Arbitrary a, Eq a, Show a, IArray arr a)
    => (Int, [a]) -> Expectation
testListArrayElems (l, xs) = do
    let n = length xs
        a = listArray @arr (l, l+n-1) xs
    elems a `shouldBe` xs

testNewListArrayGetElems
    :: forall marr a. (Arbitrary a, Eq a, Show a, MArray marr a IO)
    => (Int, [a]) -> Expectation
testNewListArrayGetElems (l, xs) = do
    let n = length xs
    a <- newListArray @marr (l, l+n-1) xs
    xs' <- getElems a
    xs' `shouldBe` xs

testReadWriteArray
    :: forall marr a. (Arbitrary a, Eq a, Show a, MArray marr a IO)
    => (Positive Int, Int, a) -> Property
testReadWriteArray (Positive n, l, x) = do
    forAll (choose (l, l+n-1)) $ \i -> do
        a <- newArray_ @marr (l, l+n-1)
        writeArray a i x
        x' <- readArray a i
        x' `shouldBe` x
