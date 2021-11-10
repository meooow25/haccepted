module ConvexHullSpec where

import Data.List

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import ConvexHull ( Point(..), clockwise, convexHull )

spec :: Spec
spec =
    prop "convexHull" $
        forAll (listOf $ Point <$> arbitrary <*> arbitrary) $ \ps ->
            convexHull ps `shouldSatisfy` verifyHull ps

verifyHull :: [Point Int] -> [Point Int] -> Bool
verifyHull ps hull = all nonHullOnRight $ zip hull (tail hull) where
    nonHullOnRight (p1, p2) = all (clockwise p1 p2) nonHull
    nonHull = ps \\ hull
