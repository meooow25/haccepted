module FenwickSpec where

import Data.Foldable
import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Fenwick

spec :: Spec
spec = do
    prop "updates, queries" $
        forAll (scale (*1000) arbitrary) $ \ft -> do
            let (l, h) = boundsF ft
            forAll (pointUpds (l, h)) $ \ivs -> do
                let ft' = applyUpdates ivs ft
                forAll (choose (l, h)) $ \j ->
                    queryF j ft' `shouldBe` naive ivs l j

    prop "updates, range queries" $
        forAll (scale (*1000) arbitrary) $ \ft -> do
            let (l, h) = boundsF ft
            forAll (pointUpds (l, h)) $ \ivs -> do
                let ft' = applyUpdates ivs ft
                    minMax i j = (min i j, max i j)
                forAll (minMax <$> choose (l, h) <*> choose (l, h)) $ \(i, j) ->
                    rangeQueryF negate i j ft' `shouldBe` naive ivs i j

    prop "range updates, queries" $
        forAll (scale (*1000) arbitrary) $ \ft -> do
            let (l, h) = boundsF ft
            forAll (rangeUpds (l, h)) $ \ijvs -> do
                let ft' = applyRangeUpdates ijvs ft
                    ivs = concat [[(i, v), (j + 1, -v)] | (i, j, v) <- ijvs]
                forAll (choose (l, h)) $ \i ->
                    queryF i ft' `shouldBe` naive ivs l i

    where
        naive ivs i j = fold [v | (k, v) <- ivs, i <= k && k <= j]
        applyUpdates ivs ft = foldl' (\ft (i, v) -> updateF i v ft) ft ivs
        applyRangeUpdates ijvs ft = foldl' (\ft (i, j, v) -> rangeUpdateF negate i j v ft) ft ijvs

instance Monoid a => Arbitrary (FTree a) where
    arbitrary = do
        n <- getSize `suchThat` (/= 0)
        n' <- choose (1, n)
        l <- arbitrary
        return $ buildF (l, l + n' - 1)

pointUpds :: (Int, Int) -> Gen [(Int, Sum Int)]
pointUpds (l, h) = listOf1 $ do
    i <- choose (l, h)
    v <- arbitrary
    return (i, Sum v)

rangeUpds :: (Int, Int) -> Gen [(Int, Int, Sum Int)]
rangeUpds (l, h) = listOf1 $ do
    i <- choose (l, h)
    j <- choose (l, h)
    v <- arbitrary
    return (min i j, max i j, Sum v)
