module PQTreeSpec where

import Data.List
import Data.Maybe

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PQTree ( buildPQ, permsPQ, reduceAllPQ )

spec :: Spec
spec = do
    prop "reduceAll" $ do
        forAll (arbitrary `suchThat` (\n -> 0 < n && n < 8)) $ \n -> do 
            let us = [1..n]
                pqt = buildPQ us
            forAll (listOf $ sublistOf us) $ \xss -> do
                maybe [] (sort . permsPQ) (reduceAllPQ xss pqt) `shouldBe` sort (naive us xss)

naive :: [Int] -> [[Int]] -> [[Int]]
naive us xss = filter ((`all` xss) . ok) pss where
    pss = permutations us
    ok ps xs = null xs || d == length xs where
        pis = zip ps [1..]
        is = map (fromJust . (`lookup` pis)) xs
        d = maximum is - minimum is + 1
