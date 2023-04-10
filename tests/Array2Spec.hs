{-# LANGUAGE TypeApplications #-}
module Array2Spec where

import Data.Array.IO
import Data.Array.Unboxed

import Test.Hspec
import Test.Hspec.QuickCheck

import Array2 ( Arr2 )
import ArrayTests ( testListArrayElems, testNewListArrayGetElems, testReadWriteArray )

spec :: Spec
spec = do
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
