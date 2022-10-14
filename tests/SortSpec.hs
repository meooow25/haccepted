module SortSpec where

import Data.Array.Unboxed

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Sort ( sort, sortBy, sortU, sortUBy, countingSort )

spec :: Spec
spec = do
    prop "sort" $
        \xs -> sort xs `shouldSatisfy` isSorted (<=)
    prop "sortBy" $
        \xs -> sortBy (flip compare) xs `shouldSatisfy` isSorted (>=)
    prop "sortU" $
        \xs -> sortU xs `shouldSatisfy` isSorted (<=)
    prop "sortUBy" $
        \xs -> sortUBy (flip compare) xs `shouldSatisfy` isSorted (>=)
    prop "countingSort" $
        forAll genArray $ \(b, xs) ->
            countingSort b id xs `shouldSatisfy` (isSorted (<=) . elems)

genArray :: Gen (Int, UArray Int Int)
genArray = do
    b <- choose (1, 256)
    xs <- listOf $ choose (0, b-1)
    pure (b, listArray (1, length xs) xs)

isSorted :: (Int -> Int -> Bool) -> [Int] -> Bool
isSorted leq xs = and $ zipWith leq xs (tail xs)
