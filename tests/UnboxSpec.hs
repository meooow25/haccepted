{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             ScopedTypeVariables, TypeFamilies, TypeApplications #-}
module UnboxSpec where

import Data.Array.IArray
import Data.Array.MArray

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Unbox ( Unbox(..), UArr, IOUArr )

spec :: Spec
spec = do
    describe "UArray" $ do
        describe "listArray and elems" $ do
            prop "X Int" $ testListArrayElems @(X Int)
            prop "Maybe Word" $ testListArrayElems @(Maybe Word)
    describe "IOUArray" $ do
        describe "newListArray and getElems" $ do
            prop "X Int" $ testNewListArrayGetElems @(X Int)
            prop "Maybe Word" $ testNewListArrayGetElems @(Maybe Word)
        describe "writeArray and readArray" $ do
            prop "X Int" $ testReadWriteArray @(X Int)
            prop "Maybe Word" $ testReadWriteArray @(Maybe Word)
  where
    testListArrayElems :: (Arbitrary a, Eq a, Show a, IArray UArr a) => (Int, [a]) -> Expectation
    testListArrayElems (l, xs) = do
        let n = length xs
            a = listArray @UArr (l, l+n-1) xs
        elems a `shouldBe` xs

    testNewListArrayGetElems :: (Arbitrary a, Eq a, Show a, MArray IOUArr a IO)
                             => (Int, [a]) -> Expectation
    testNewListArrayGetElems (l, xs) = do
        let n = length xs
        a <- newListArray @IOUArr (l, l+n-1) xs
        xs' <- getElems a
        xs' `shouldBe` xs

    testReadWriteArray :: (Arbitrary a, Eq a, Show a, MArray IOUArr a IO)
                       => (Positive Int, Int, a) -> Property
    testReadWriteArray (Positive n, l, x) = do
        forAll (choose (l, l+n-1)) $ \i -> do
            a <- newArray_ @IOUArr (l, l+n-1)
            writeArray a i x
            x' <- readArray a i
            x' `shouldBe` x

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
