module SparseTableSpec where

import Data.Array
import Data.Bits
import Data.Monoid
import Data.Semigroup

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SparseTable ( fromArraySP, fromListSP, query1SP, querySP )
import Util ( genSortedIntPair )

spec :: Spec
spec = do
    let testFromArray gen = forAll gen $ \xa -> do
            let st = fromArraySP xa
            forAll (choose $ bounds st) $ \j ->
                forAll (choose $ bounds $ st!j) $ \i ->
                    st!j!i `shouldBe` naive xa i (i + 1 `shiftL` j - 1)

        testFromList gen = forAll gen $ \xs -> do
            let n = length xs
                st = fromListSP (1, n) xs
                xa = listArray (1, n) xs
            forAll (choose $ bounds st) $ \j ->
                forAll (choose $ bounds $ st!j) $ \i ->
                    st!j!i `shouldBe` naive xa i (i + 1 `shiftL` j - 1)

        testQuery qf gen = forAll gen $ \xa -> do
            let bnds = bounds xa
                st = fromArraySP xa
            forAll (genSortedIntPair bnds) $ \(i, j) ->
                qf i j st `shouldBe` naive xa i j

    prop "fromArraySP sum" $ testFromArray $ genXa Sum
    prop "fromArraySP max" $ testFromArray $ genXa Max

    prop "fromListSP sum" $ testFromList $ genXs Sum
    prop "fromListSP max" $ testFromList $ genXs Max

    prop "querySP sum" $ testQuery querySP $ genXa Sum 
    prop "querySP max" $ testQuery querySP $ genXa Max

    prop "query1SP max" $ testQuery query1SP $ genXa Max 

  where
    naive xa l r = foldMap (xa!) [l..r]

genXa :: (Int -> a) -> Gen (Array Int a)
genXa f = do
    xs <- arbitrary `suchThat` (not . null)
    l <- arbitrary
    let n = length xs
        xa = listArray (l, l + n - 1) $ map f xs
    pure xa

genXs :: (Int -> a) -> Gen [a]
genXs f = do
    xs <- arbitrary `suchThat` (not . null)
    pure $ map f xs
