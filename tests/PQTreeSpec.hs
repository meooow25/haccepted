module PQTreeSpec where

import Data.List
import Data.Maybe

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import PQTree ( buildPQ, permsPQ, reduceAllPQ )

spec :: Spec
spec = do
    prop "reduceAll" $
        forAll genUs $ \us -> do
            let pqt = buildPQ us
            forAll (listOf $ sublistOf us) $ \xss -> do
                maybe [] (sort . permsPQ) (reduceAllPQ xss pqt) `shouldBe` sort (naive us xss)

genUs :: Gen [Int]
genUs = sized $ \n' -> do
    n <- choose (1, max 1 $ min 6 n')
    pure [1..n]

naive :: [Int] -> [[Int]] -> [[Int]]
naive us xss = filter ((`all` xss) . ok) pss where
    pss = permutations us
    ok ps xs = null xs || d == length xs where
        pis = zip ps [1..]
        is = map (fromJust . (`lookup` pis)) xs
        d = maximum is - minimum is + 1
