{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             ScopedTypeVariables, TypeFamilies, TypeApplications #-}
module ArraySpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Array ( Unbox(..), UArr, IOUArr )
import ArrayTests ( testListArrayElems, testNewListArrayGetElems, testReadWriteArray )

spec :: Spec
spec = do
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
