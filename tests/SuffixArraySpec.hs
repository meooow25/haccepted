{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module SuffixArraySpec where

import Data.Array.Unboxed
import Data.List
import qualified Data.ByteString.Char8 as C

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SuffixArray ( buildSufA, buildSufAL )
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
            let (sa, lcp) = buildSufA 128 (C.length s) (fromEnum . C.index s)
            (elems sa, elems lcp) `shouldBe` sufANaive (C.unpack s)

    testBuildLarge = \xs -> do
        let n = length xs
            a = listArray @UArray (0, n-1) xs
            (sa, lcp) = buildSufAL n (a!)
        (elems sa, elems lcp) `shouldBe` sufANaive (elems a)

sufANaive :: Ord a => [a] -> ([Int], [Int])
sufANaive s = (p, lcp) where
    p = sortOn (`drop` s) [0 .. length s - 1]
    lcp = zipWith getlcp <*> tail $ map (`drop` s) p
    getlcp xs ys = length $ takeWhile id $ zipWith (==) xs ys
