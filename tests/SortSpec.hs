{-# LANGUAGE ScopedTypeVariables #-}
module SortSpec where

import Data.Array.Unboxed
import Data.Ord
import qualified Data.List as L

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Sort ( sort, sortBy, sortU, sortUBy, sortUABy, countingSortUA )

spec :: Spec
spec = do
    prop "sort" $
        forAll genWithIdxs $ \xs ->
            sort xs `shouldBe` L.sort xs
    prop "sortBy" $
        forAll genWithIdxs $ \xs ->
            sortBy (flip compare) xs `shouldBe` L.sortBy (flip compare) xs
    prop "sortU" $
        \(xs :: [Int]) -> sortU xs `shouldBe` L.sort xs
    prop "sortUBy" $
        \(xs :: [Int]) -> do
            let xa = listArray (1, length xs) xs :: UArray Int Int
                is = range (bounds xa)
            sortUBy (comparing (xa!)) is `shouldBe` L.sortBy (comparing (xa!)) is
    prop "sortUABy" $
        forAll genArray $ \(_, xa) -> do
            let ia = listArray (bounds xa) (range (bounds xa))
            elems (sortUABy (comparing (xa!)) ia) `shouldBe` L.sortBy (comparing (xa!)) (elems ia)
    prop "countingSortUA" $
        forAll genArray $ \(b, xa) -> do
            let ia = listArray (bounds xa) (range (bounds xa))
            elems (countingSortUA b (xa!) ia) `shouldBe` L.sortBy (comparing (xa!)) (elems ia)

-- These tests check that the sorts are stable (except for sortU) by comparing against the known
-- stable Data.List.sort.
-- This is done by using WithIdx for boxed sorts and sorting indexes for unboxed ones.

data WithIdx a = WithIdx !a !Int deriving (Eq, Show)

-- Yes, this is unlawful wrt Eq.
instance Ord a => Ord (WithIdx a) where
    WithIdx x _ `compare` WithIdx y _ = compare x y

genWithIdxs :: Gen [WithIdx Int]
genWithIdxs = zipWith (flip WithIdx) [0..] <$> arbitrary

genArray :: Gen (Int, UArray Int Int)
genArray = do
    b <- getPositive <$> arbitrary
    l <- arbitrary
    xs <- listOf (choose (0, b-1))
    pure (b, listArray (l, l + length xs - 1) xs)
