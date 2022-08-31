module AhoCorasickSpec where

import Data.List
import Data.Ord
import qualified Data.ByteString.Char8 as C

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import AhoCorasick ( fromListTAC, fromTrieAC, matchAC )

spec :: Spec
spec = do
    prop "build and match" $
        forAll ((,) <$> genBS <*> listOf genBS) $ \(s, ps) -> do
            let ac = fromTrieAC $ fromListTAC $ zip ps [1..]
                expected = naiveMatch ps s
            label ("num matches " ++ labelLen (length (concat expected))) $
                matchAC ac s `shouldBe` expected

labelLen :: Int -> String
labelLen n
    | n == 0    = " = 0"
    | n <= 10   = "<= 10"
    | n <= 100  = "<= 100"
    | otherwise = " > 100"

genBS :: Gen C.ByteString
genBS = C.pack . getASCIIString <$> arbitrary

naiveMatch :: [C.ByteString] -> C.ByteString -> [[Int]]
naiveMatch ps = map match . C.inits where
    ps' = reverse $ -- reverse required to match order
        sortBy (comparing (C.length . fst)) $ zip ps [1..]
    match s' = [i | (p, i) <- ps', p `C.isSuffixOf` s']
