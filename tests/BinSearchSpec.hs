module BinSearchSpec where

import Control.Monad
import Data.Int
import Data.List

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import BinSearch

spec :: Spec
spec = do
    describe "binSearch" $ do
        it "fixed small" $ do
            let l = 10 :: Int
                h = 30
                x = 25
            binSearch ((>=x) . validate l h) l h `shouldBe` x
        it "fixed large" $ do
            let l = 123 :: Int64
                h = 10 ^ (18 :: Int)
                x = 10 ^ (17 :: Int)
            binSearch ((>=x) . validate l h) l h `shouldBe` x

        prop "empty" $
            \l -> binSearch undefined l l `shouldBe` (l :: Int)
        prop "random" $
            forAll (sortedList 3) $
                \[l, x, h] -> binSearch ((>=x) . validate l h) l h `shouldBe` x
        prop "always False" $
            forAll (sortedList 2) $
                \[l, h] -> binSearch (const False . validate l h) l h `shouldBe` h
        prop "always True" $
            forAll (sortedList 2) $
                \[l, h] -> binSearch (const True . validate l h) l h `shouldBe` l

validate :: Integral i => i -> i -> i -> i
validate l h i
    | i < l || i >= h = error "outside expected range"
    | otherwise = i

sortedList :: Int -> Gen [Int]
sortedList n = fmap sort $ replicateM n $ choose (-10 ^ (9 :: Int), 10 ^ (9 :: Int))
