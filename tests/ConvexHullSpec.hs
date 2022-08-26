module ConvexHullSpec where

import Data.List

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import ConvexHull ( convexHull )
import Geometry ( V2(..), turn )

spec :: Spec
spec =
    prop "convexHull" $
        forAll (listOf $ V2 <$> arbitrary <*> arbitrary) $ \ps ->
            convexHull ps `shouldSatisfy` verifyHull ps

verifyHull :: [V2] -> [V2] -> Bool
verifyHull ps hull = and $ zipWith nonHullOnRight hull (tail hull) where
    nonHullOnRight p1 p2 = all ((/=LT) . turn p1 p2) nonHull
    nonHull = ps \\ hull
