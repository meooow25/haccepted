module SparseTableSpec where

import Data.Semigroup

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SparseTable ( fromListSP, fromListISP, fromListUSP, fromListIUSP )
import Util ( genSortedIntPair )

spec :: Spec
spec = do
    prop "fromListSP Sum"   $ testfl fromListSP Sum
    prop "fromListSP Max"   $ testfl fromListSP Max
    prop "fromListSP First" $ testfl fromListSP First

    prop "fromListISP Max"   $ testfl fromListISP Max
    prop "fromListISP First" $ testfl fromListISP First

    prop "fromListUSP (+)"   $ testUfl fromListUSP (+)
    prop "fromListUSP max"   $ testUfl fromListUSP max
    prop "fromListUSP const" $ testUfl fromListUSP const

    prop "fromListIUSP max"   $ testUfl fromListIUSP max
    prop "fromListIUSP const" $ testUfl fromListIUSP const
  where
    testfl fl f = forAll gen $ \((l, r), xs, (i, j)) ->
        fl (l, r) (map f xs) i j `shouldBe` naive (<>) (map f xs) (i - l) (j - l)
    testUfl fl op = forAll gen $ \((l, r), xs, (i, j)) ->
        fl op (l, r) xs i j `shouldBe` naive op xs (i - l) (j - l)
    naive f xs i j = foldl1 f $ map (xs!!) [i..j]

gen :: Gen ((Int, Int), [Int], (Int, Int))
gen = do
    l <- arbitrary
    xs <- arbitrary `suchThat` (not . null)
    let r = l + length xs - 1
    ij <- genSortedIntPair (l, r)
    pure ((l, r), xs, ij)
