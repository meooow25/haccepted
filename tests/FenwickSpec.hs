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
        forAll (sizedFt LargeS) $ \ft -> do
            let (l, h) = boundsF ft
            forAll (indexVal (l, h)) $ \(i, v) -> do
                let ft' = updateF i v ft
                forAll (choose (l, h)) $ \j ->
                    queryF j ft' `shouldBe` if j < i then mempty else v

    prop "small, random updates, all queries" $
        forAll (sizedFt SmallS) $ \ft -> do
            let (l, h) = boundsF ft
            forAll (listOf1 $ indexVal (l, h)) $ \ivs -> do
                let ft' = applyUpdates ivs ft
                    sa = prefixSum (l, h) ivs
                forM_ [l..h] $ \j ->
                    queryF j ft' `shouldBe` sa!j

    prop "small, random updates, random range queries" $
        forAll (sizedFt SmallS) $ \ft -> do
            let (l, h) = boundsF ft
            forAll (listOf1 $ indexVal (l, h)) $ \ivs -> do
                let ft' = applyUpdates ivs ft
                    sa = prefixSum (l, h) ivs
                    get i = if i < l then mempty else sa!i
                forAll (choose (l, h)) $ \j ->
                    forAll (choose (l, h)) $ \k -> do
                        let (j', k') = if j < k then (j, k) else (k, j)
                        rangeQueryF negate j' k' ft' `shouldBe` get k' - get (j' - 1)

    prop "very small, random updates, all range queries" $
        forAll (sizedFt VerySmallS) $ \ft -> do
            let (l, h) = boundsF ft
            forAll (listOf1 $ indexVal (l, h)) $ \ivs -> do
                let ft' = applyUpdates ivs ft
                    sa = prefixSum (l, h) ivs
                    get i = if i < l then mempty else sa!i
                forM_ [l..h] $ \j ->
                    forM_ [j..h] $ \k ->
                        rangeQueryF negate j k ft' `shouldBe` get k - get (j - 1)

    prop "large, single range update, random queries" $
        forAll (sizedFt LargeS) $ \ft -> do
            let (l, h) = boundsF ft
            forAll (rangeVal (l, h)) $ \(i, j, v) -> do
                let ft' = rangeUpdateF negate i j v ft
                forAll (choose (l, h)) $ \k ->
                    queryF k ft' `shouldBe` if i <= k && k <= j then v else mempty

    prop "small, random range updates, all queries" $
        forAll (sizedFt SmallS) $ \ft -> do
            let (l, h) = boundsF ft
            forAll (listOf1 $ rangeVal (l, h)) $ \ijvs -> do
                let ft' = applyRangeUpdates ijvs ft
                    sa = prefixSum (l, h + 1) $ concat [[(i, v), (j + 1, -v)] | (i, j, v) <- ijvs]
                forM_ [l..h] $ \i ->
                    queryF i ft' `shouldBe` sa!i

    where
        prefixSum (l, h) ivs = listArray (l, h) $ scanl1 (<>) $ elems $ accumArray (<>) mempty (l, h + 1) ivs
        applyUpdates ivs ft = foldl' (\ft (i, v) -> updateF i v ft) ft ivs
        applyRangeUpdates ijvs ft = foldl' (\ft (i, j, v) -> rangeUpdateF negate i j v ft) ft ijvs

data Size = VerySmallS | SmallS | LargeS

instance Monoid a => Arbitrary (FTree a) where
    arbitrary = do
        n <- getSize `suchThat` (/= 0)
        n' <- choose (1, n)
        l <- scale (*100) arbitrary
        let h = l + n' - 1
            ft = buildF (l, h)
        return ft

sizedFt :: Monoid a => Size -> Gen (FTree a)
sizedFt size = do
    let n = case size of
            VerySmallS -> 200
            SmallS -> 2000
            LargeS -> 10 ^ (6 :: Int)
    resize n arbitrary

indexVal :: (Int, Int) -> Gen (Int, Sum Int)
indexVal (l, h) = do
    i <- choose (l, h)
    v <- arbitrary
    return (i, Sum v)

rangeVal :: (Int, Int) -> Gen (Int, Int, Sum Int)
rangeVal (l, h) = do
    i <- choose (l, h)
    j <- choose (l, h)
    v <- arbitrary
    return (min i j, max i j, Sum v)
