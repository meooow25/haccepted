module SuffixTreeBench where

import Data.List
import qualified Data.ByteString.Char8 as C

import Criterion

import SuffixTree ( SufTNode, buildSufT, matchSufT )
import Util ( evalR, randLowerCaseString, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "SuffixTree"
    [ -- Build a suffix tree from an a-z string of length n.
      bgroup "build" $ map benchBuild sizes

      -- Query 100 patterns, each equal to the a-z string the suffix tree was constructed with.
    , bgroup "match" $ map benchMatch matchSizes
    ]

sizes :: [Int]
sizes = [100, 10000, 200000]

numQueries :: Int
numQueries = 100

matchSizes :: [Int]
matchSizes = map (*numQueries) sizes

benchBuild :: Int -> Benchmark
benchBuild n = sizedBench n gen $ \s -> nf (buildCountSufT (C.length s)) (fromEnum . C.index s) where
    gen = evalR $ C.pack <$> randLowerCaseString n

benchMatch :: Int -> Benchmark 
benchMatch n = sizedBench n gen $ \(s, st, ts) -> nf (go s st) ts where
    gen = evalR $ do
        let n' = div n numQueries
        s <- C.pack <$> randLowerCaseString n'
        let st = buildCountSufT (C.length s) (fromEnum . C.index s)
        pure (s, st, replicate numQueries s)
    go s st = foldl' (\_ t -> match t `seq` ()) () where
        match t = matchSufT const (fromEnum . C.index s) st (C.length t) (fromEnum . C.index t)

buildCountSufT :: Int -> (Int -> Int) -> SufTNode Int
buildCountSufT = buildSufT (const 1) const (+)
