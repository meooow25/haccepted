module SuffixTreeBench where

import Data.List
import qualified Data.ByteString.Char8 as C

import Criterion

import SuffixTree ( SuffixTree, buildSufT, matchSufT )
import Util ( evalR, randLowerCaseString, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "SuffixTree"
    [ -- Build a suffix tree from a a-z string of length n.
      bgroup "build" $ map benchBuild sizes

      -- Query n/50 patterns, each equal to the a-z string the suffix tree was constructed with.
    , bgroup "match" $ map benchMatch matchSizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

numMatches :: Int
numMatches = 50

matchSizes :: [Int]
matchSizes = map (*numMatches) sizes

benchBuild :: Int -> Benchmark
benchBuild n = sizedBench n gen $ nf buildCountSufTree where
    gen = evalR $ C.pack <$> randLowerCaseString n

benchMatch :: Int -> Benchmark 
benchMatch n = sizedBench n gen $ \(st, ts) -> nf (go st) ts where
    gen = evalR $ do
        let n' = div n numMatches
        s <- C.pack <$> randLowerCaseString n'
        let st = buildCountSufTree s
        pure (st, replicate numMatches s)
    go st = foldl' (\_ t -> matchSufT st t `seq` ()) ()

buildCountSufTree :: C.ByteString -> SuffixTree Int
buildCountSufTree = buildSufT (const (1 :: Int)) const (+)
