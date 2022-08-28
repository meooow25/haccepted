module ConvexHullSpec where

import Data.List

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import ConvexHull ( convexHull )
import Geometry ( V2(..), dist2, turn )

spec :: Spec
spec =
    prop "convexHull" $
        forAll (listOf $ V2 <$> arbitrary <*> arbitrary) $ \ps ->
            convexHull ps `shouldBe` jarvisMarch ps

jarvisMarch :: [V2] -> [V2]
jarvisMarch [] = []
jarvisMarch ps = go pmin [] where
    pmin = minimum ps
    go p hull = if q == pmin then p:hull else go q (p:hull) where
        q = maximumBy cmp ps
        cmp p1 p2 = turn p p2 p1 <> compare (dist2 p p1) (dist2 p p2)
