{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             ScopedTypeVariables, TypeFamilies, TypeApplications #-}
module ArraySpec where

import Data.Array.IO
import Data.Array.Unboxed

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Array ( Arr2, IOUArr, UArr, Unbox(..) )
import ArrayTests ( testListArrayElems, testNewListArrayGetElems, testReadWriteArray )

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
