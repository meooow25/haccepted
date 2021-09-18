module BinSearchSpec where

import Control.Monad.Identity
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import BinSearch ( binSearch, binSearchM )
import Util ( genSortedIntPair )

spec :: Spec
spec = do
    let testBinSearch bs = do
            prop "empty" $ \l ->
                bs undefined l (l - 1) `shouldBe` (l :: Int)
            prop "random" $
                forAll (genSortedIntPair bnds) $ \(l, h) ->
                    forAll (choose (l, h)) $ \x ->
                        bs ((>=x) . validate l h) l h `shouldBe` x
            prop "always False" $
                forAll (genSortedIntPair bnds) $ \(l, h) ->
                    bs (const False . validate l h) l h `shouldBe` h + 1
            prop "always True" $
                forAll (genSortedIntPair bnds) $ \(l, h) ->
                    bs (const True . validate l h) l h `shouldBe` l

    describe "binSearch" $ testBinSearch binSearch
    describe "binSearchM" $
        testBinSearch $ \f l h -> runIdentity $ binSearchM (Identity . f) l h

validate :: Integral i => i -> i -> i -> i
validate l h i
    | i < l || h < i = error "outside expected range"
    | otherwise      = i

bnds :: (Int, Int)
bnds = (-lim, lim) where lim = 10 ^ (9 :: Int)
