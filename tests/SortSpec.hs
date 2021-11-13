module SortSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Sort ( sort, sortBy, sortU, sortUBy )

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

isSorted :: (Int -> Int -> Bool) -> [Int] -> Bool
isSorted leq xs = and $ zipWith leq xs (tail xs)
