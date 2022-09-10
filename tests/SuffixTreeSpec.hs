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
import TreeDebugUtil

spec :: Spec
spec = do
    prop "build, check structure" $
        forAll genBS $ \s -> do
            let st = buildSufT (const ()) const const s
                st' = naiveBuild (const ()) const const s
            expectEqSufTrees st st'

    prop "build, check structure and value" $
        forAll genBS $ \s -> do
            let st = buildSufT Leaf EdgeUpd Merge s
                st' = naiveBuild Leaf EdgeUpd Merge s
            expectEqSufTrees st st'

genBS :: Gen C.ByteString
-- genBS = C.pack . getASCIIString <$> arbitrary
genBS = C.pack <$> listOf (elements "01")

deriving instance Eq a => Eq (SufTreeNode a)
deriving instance Eq a => Eq (SufTreeEdge a)

expectEqSufTrees :: (Eq a, Show a) => SufTreeNode a -> SufTreeNode a -> Expectation
expectEqSufTrees act exp = unless (act == exp) $ expectationFailure $ unlines
    [ "actual:"
    , drawSufTreeNode act
    , "expected:"
    , drawSufTreeNode exp
    ]

drawSufTreeNode :: Show a => SufTreeNode a -> String
drawSufTreeNode = draw (show . valSufT) nbs where
    nbs (SufTreeNode _ nxt) = [(show (left, len), v)| SufTreeEdge left len v <- IM.elems nxt]
    nbs _ = []

data SufTreeTrace
    = Leaf !Int
    | EdgeUpd !SufTreeTrace !Int
    | Merge !SufTreeTrace !SufTreeTrace
    deriving (Show, Eq)

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
