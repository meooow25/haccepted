module SparseTableSpec where

import Data.Array
import Data.Bits
import Data.Monoid
import Data.Semigroup

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SparseTable

spec :: Spec
spec = do
    let testBuild gen = forAll (scale (*100) gen) $ \xa -> do
            let st = fromArraySP xa
            forAll (choose $ bounds st) $ \j ->
                forAll (choose $ bounds $ st!j) $ \i ->
                    st!j!i `shouldBe` naive xa i (i + 1 `shiftL` j - 1)

        testQuery qf gen = forAll (scale (*100) gen) $ \xa -> do
            let bnds = bounds xa
                st = fromArraySP xa
                minMax i j = (min i j, max i j)
            forAll (minMax <$> choose bnds <*> choose bnds) $ \(i, j) ->
                qf i j st `shouldBe` naive xa i j

    prop "fromArraySP sum" $ testBuild $ genXs Sum
    prop "fromArraySP max" $ testBuild $ genXs Max

    prop "querySP sum" $ testQuery querySP $ genXs Sum 
    prop "querySP max" $ testQuery querySP $ genXs Max

    prop "query1SP max" $ testQuery query1SP $ genXs Max 

    where
        naive xa l r = foldMap (xa!) [l..r]

genXs :: Semigroup a => (Int -> a) -> Gen (Array Int a)
genXs f = do
    xs <- arbitrary `suchThat` (not . null)
    l <- arbitrary
    let n = length xs
        xa = listArray (l, l + n - 1) $ map f xs
    return xa
