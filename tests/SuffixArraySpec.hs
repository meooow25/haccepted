{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module SuffixArraySpec where

import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as C

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SuffixArray ( buildSufArray, buildSufArrayL )
import Util ( genBinaryBS, genASCIIBS )

spec :: Spec
spec = do
    prop "build binary" $
        testBuild genBinaryBS
    prop "build ASCII" $
        testBuild genASCIIBS
    prop "build large alphabet" $
        testBuildLarge
  where
    testBuild gen =
        forAll gen $ \s -> do
            let sa = buildSufArray 128 (C.length s) (fromEnum . C.index s)
                checkSufArray = isSorted . map (`C.drop` s) . elems
            sa `shouldSatisfy` checkSufArray

    testBuildLarge = \xs -> do
        let n = length xs
            a = listArray @UArray (0, n-1) xs
            sa = buildSufArrayL n (a!)
            checkSufArray = isSorted . map (`drop` elems a) . elems
        sa `shouldSatisfy` checkSufArray

isSorted :: Ord a => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs (tail xs)
