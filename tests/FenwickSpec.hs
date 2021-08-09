module FenwickSpec where

import Control.Monad
import Data.Array
import Data.List
import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Fenwick

spec :: Spec
spec = do
    prop "large, single update, random queries" $
        forAll (genFt High) $ \(l, h, ft) ->
            forAll (indexVal (l, h)) $ \(i, v) -> do
                let ft' = updateF i v ft
                forAll (choose (l, h)) $ \j ->
                    queryF j ft' `shouldBe` if j < i then mempty else v

    prop "small, single update, all queries" $
        forAll (genFt Low) $ \(l, h, ft) ->
            forAll (indexVal (l, h)) $ \(i, v) -> do
                let ft' = updateF i v ft
                forM_ [l..h] $ \j ->
                    queryF j ft' `shouldBe` if j < i then mempty else v

    prop "small, random updates, all queries" $
        forAll (genFt Low) $ \(l, h, ft) ->
            forAll (listOf1 $ indexVal (l, h)) $ \ivs -> do
                let ft' = foldl' (\f (i, v) -> updateF i v f) ft ivs
                    ss = scanl1 (<>) $ elems $ accumArray (<>) mempty (l, h) ivs
                forM_ (zip [l..h] ss) $ \(j, s) ->
                    queryF j ft' `shouldBe` s

    prop "small, random updates, random range queries" $
        forAll (genFt Low) $ \(l, h, ft) ->
            forAll (listOf1 $ indexVal (l, h)) $ \ivs -> do
                let ft' = foldl' (\f (i, v) -> updateF i v f) ft ivs
                    sa = listArray (l, h) $ scanl1 (<>) $ elems $ accumArray (<>) mempty (l, h) ivs
                    getS i = if i < l then mempty else sa!i
                forAll (choose (l, h)) $ \j ->
                    forAll (choose (l, h)) $ \k -> do
                        let (j', k') = if j < k then (j, k) else (k, j)
                        rangeQueryF negate j' k' ft' `shouldBe` (getS k' - getS j')

    prop "very small, random updates, all range queries" $
        forAll (genFt VeryLow) $ \(l, h, ft) ->
            forAll (listOf1 $ indexVal (l, h)) $ \ivs -> do
                let ft' = foldl' (\f (i, v) -> updateF i v f) ft ivs
                    sa = listArray (l, h) $ scanl1 (<>) $ elems $ accumArray (<>) mempty (l, h) ivs
                    getS i = if i < l then mempty else sa!i
                forM_ [l..h] $ \j ->
                    forM_ [j..h] $ \k ->
                        rangeQueryF negate j k ft' `shouldBe` (getS k - getS j)

indexVal :: (Int, Int) -> Gen (Int, Sum Int)
indexVal (l, h) = do
    i <- choose (l, h)
    v <- arbitrary
    return (i, Sum v)

data Size = VeryLow | Low | High

genFt :: Monoid a => Size -> Gen (Int, Int, FTree a)
genFt size = do
    let bndsLim = 10 ^ (9 :: Int)
        sizeLim = case size of
            VeryLow -> 200
            Low -> 2000
            High -> 10 ^ (6 :: Int)
    l <- choose (-bndsLim, bndsLim)
    s <- choose (sizeLim `div` 2, sizeLim)
    let h = l + s - 1
        ft = buildF (l, h)
    return (l, h, ft)
