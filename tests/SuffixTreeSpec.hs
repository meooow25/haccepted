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

import SuffixTree ( SufTNode(..), SufTEdge(..), buildSufT, buildMatchSufT, drawSufTNode )
import Misc ( unique )
import Util ( genBinaryBS, genASCIIBS )

spec :: Spec
spec = do
    prop "build binary" $
        testBuild genBinaryBS
    prop "distinct prefix match binary" $
        testDistinctPrefixMatch genBinaryBS
    prop "build ASCII" $
        testBuild genASCIIBS
    prop "distinct prefix match ASCII" $
        testDistinctPrefixMatch genASCIIBS
  where
    testBuild gen =
        forAll gen $ \s -> do
            let st = buildSufT Leaf EdgeUpd Merge (C.length s) (fromEnum . C.index s)
                st' = naiveBuild Leaf EdgeUpd Merge s
            st `shouldBeEqSufNode` st'

    -- Example query, count number of distinct substrings of s that have t as prefix
    testDistinctPrefixMatch gen =
        forAll ((,) <$> gen <*> gen) $ \(s, t) -> do
            let match = buildMatchSufT (const 0) (+) (+) (C.length s) (fromEnum . C.index s)
                act = (+1) <$> match (C.length t) (fromEnum . C.index t)
                exp = numDistinctPrefixMatch s t
            classify (isJust exp) "matched" $
                act `shouldBe` exp

deriving instance Eq a => Eq (SufTNode a)
deriving instance Eq a => Eq (SufTEdge a)

shouldBeEqSufNode :: (Eq a, Show a) => SufTNode a -> SufTNode a -> Expectation
shouldBeEqSufNode got exp = unless (got == exp) $ expectationFailure $ unlines
    [ "expected:"
    , drawSufTNode exp
    , "but got:"
    , drawSufTNode got
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

naiveBuild :: (Int -> a) -> (a -> Int -> a) -> (a -> a -> a) -> C.ByteString -> SufTNode a
naiveBuild fromLeaf updEdge merge s = go (C.length s) (sortBy (comparing str_) $ suffixes s) where
    go dep [] = SufTNode (fromLeaf dep) IM.empty
    go dep sufs = SufTNode a nxt where
        nxt = IM.fromList $ mkEdge <$> groupByFirst sufs
        mkEdge (c, sufs) = (fromEnum c, SufTEdge left len v) where
            left = minimum (map start_ sufs)
            (len, sufs') = trimLCP sufs
            v = go (dep - len) sufs'
        a = foldl1' merge [updEdge a' len | SufTEdge _ len (SufTNode a' _) <- IM.elems nxt]

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
