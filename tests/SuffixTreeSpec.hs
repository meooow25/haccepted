{-# LANGUAGE StandaloneDeriving #-}
module SuffixTreeSpec where

import Control.Monad
import Data.Bifunctor
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap as IM

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import SuffixTree
import Misc ( unique )

spec :: Spec
spec = do
    prop "build binary" $
        testBuild genBinary
    prop "distinct prefix match binary" $
        testDistinctPrefixMatch genBinary
    prop "build ASCII" $
        testBuild genASCII
    prop "distinct prefix match ASCII" $
        testDistinctPrefixMatch genASCII
  where
    testBuild gen =
        forAll gen $ \s -> do
            let SuffixTree _ _ st = buildSufT Leaf EdgeUpd Merge (C.length s) (fromEnum . C.index s)
                st' = naiveBuild Leaf EdgeUpd Merge s
            st `shouldBeEqSufNode` st'

    -- Example query, count number of distinct substrings of s that have t as prefix
    testDistinctPrefixMatch gen =
        forAll ((,) <$> gen <*> gen) $ \(s, t) -> do
            let st = buildSufT (const 0) (+) (+) (C.length s) (fromEnum . C.index s)
                act = (+1) <$> matchSufT st (C.length t) (fromEnum . C.index t)
                exp = numDistinctPrefixMatch s t
            classify (isJust exp) "matched" $
                act `shouldBe` exp

genBinary, genASCII :: Gen C.ByteString
genBinary = C.pack <$> listOf (elements "01")
genASCII = C.pack . getASCIIString <$> arbitrary

deriving instance Eq a => Eq (SufTreeNode a)
deriving instance Eq a => Eq (SufTreeEdge a)

shouldBeEqSufNode :: (Eq a, Show a) => SufTreeNode a -> SufTreeNode a -> Expectation
shouldBeEqSufNode got exp = unless (got == exp) $ expectationFailure $ unlines
    [ "expected:"
    , drawSufTreeNode exp
    , "but got:"
    , drawSufTreeNode got
    ]

data SufTreeTrace
    = Leaf !Int
    | EdgeUpd !SufTreeTrace !Int
    | Merge !SufTreeTrace !SufTreeTrace
    deriving (Show, Eq)

numDistinctPrefixMatch :: C.ByteString -> C.ByteString -> Maybe Int
numDistinctPrefixMatch s t = if cnt == 0 then Nothing else Just cnt where
    subs = unique $ sort $ C.tails s >>= C.inits
    cnt = length $ filter (t `C.isPrefixOf`) subs

----------------
-- Naive build

data Suffix = Suffix { start_ :: !Int, str_ :: !C.ByteString } deriving Show

suffixTail :: Suffix -> Maybe Suffix
suffixTail (Suffix start s)
    | C.null s' = Nothing
    | otherwise = Just (Suffix (start + 1) s')
  where
    s' = C.tail s

naiveBuild :: (Int -> a) -> (a -> Int -> a) -> (a -> a -> a) -> C.ByteString -> SufTreeNode a
naiveBuild fromLeaf updEdge merge s = go (C.length s) (sortBy (comparing str_) $ suffixes s) where
    go dep [] = SufTreeNode (fromLeaf dep) IM.empty
    go dep csufs = SufTreeNode a nxt where
        nxt = IM.fromList
            [ (fromEnum c, SufTreeEdge mini lcp child)
            | (c, sufs) <- groupByFirst csufs
            , let mini = minimum (map start_ sufs)
                  (lcp, sufs') = trimLCP sufs
                  child = go (dep - lcp) sufs'
            ]
        a = foldl1' merge [updEdge (valSufT v) len | SufTreeEdge _ len v <- IM.elems nxt]

suffixes :: C.ByteString -> [Suffix]
suffixes = zipWith Suffix [0..] . init . C.tails

groupByFirst :: [Suffix] -> [(Char, [Suffix])]
groupByFirst = map (C.head . str_ . head >>= (,)) . groupBy ((==) `on` (C.head . str_))

trimLCP :: [Suffix] -> (Int, [Suffix])
trimLCP [] = (0, [])
trimLCP sufs@(suf:_)
    | headMatch = first (+1) $ trimLCP $ mapMaybe suffixTail sufs
    | otherwise = (0, sufs)
  where
    headMatch = all ((== C.head (str_ suf)) . C.head . str_) sufs
