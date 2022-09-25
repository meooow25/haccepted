module AhoCorasickSpec where

import Data.List
import Data.Ord
import qualified Data.ByteString.Char8 as C

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import AhoCorasick ( fromListTAC, fromTrieAC, matchAC )
import Util ( genBinaryBS, genASCIIBS )

spec :: Spec
spec = do
    prop "build and match binary" $
        testAC genBinaryBS
    prop "build and match ASCII" $
        testAC genASCIIBS
  where
    testAC gen =
        forAll ((,) <$> listOf gen <*> gen) $ \(ps, s) -> do
            let ac = fromTrieAC $ fromListTAC $ zip ps [0..]
                expected = naiveMatch ps s
            label (labelNumMatches expected) $
                label (labelNonSimpleMatches expected ps) $
                    matchAC ac s `shouldBe` expected

labelNumMatches :: [[Int]] -> String
labelNumMatches iss = "num matches " ++ bucket (length (concat iss))

labelNonSimpleMatches :: [[Int]] -> [C.ByteString] -> String
labelNonSimpleMatches iss ps = "num (len >1) matches " ++ bucket cnt where
    cnt = length $ filter ((>1) . C.length . (ps!!)) $ concat iss

bucket :: Int -> String
bucket n
    | n == 0    = " = 0"
    | n <= 10   = "<= 10"
    | n <= 100  = "<= 100"
    | otherwise = " > 100"

naiveMatch :: [C.ByteString] -> C.ByteString -> [[Int]]
naiveMatch ps = map match . C.inits where
    ps' = reverse $ -- reverse required to match order
        sortBy (comparing (C.length . fst)) $ zip ps [0..]
    match s' = [i | (p, i) <- ps', p `C.isSuffixOf` s']
