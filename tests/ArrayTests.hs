{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, TypeApplications #-}
module ArrayTests where

import Data.Array.IArray
import Data.Array.MArray

import Test.Hspec
import Test.QuickCheck

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
