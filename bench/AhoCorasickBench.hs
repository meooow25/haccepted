module AhoCorasickBench where

import Control.Monad
import qualified Data.ByteString.Char8 as C

import Criterion

import AhoCorasick
import Util ( RandStd, evalR, randLowerCaseString, sizedBench )

benchmark :: Benchmark
benchmark = bgroup "AhoCorasick"
    [ -- Build the Aho-Corasick automaton from 20 a-z strings of length n/20.
      bgroup "build from few long" $ map (benchBuild genFew) sizes

      -- Match an Aho-Corasick automaton built from 20 a-z strings of length n/20 on a string of
      -- length n.
    , bgroup "match few long" $ map (benchMatch genFew) sizes

      -- Build the Aho-Corasick automaton from n/20 a-z strings of length 20.
    , bgroup "build from many short" $ map (benchBuild genMany) sizes

      -- Match an Aho-Corasick automaton built from n/20 a-z strings of length 20 on a string of
      -- length n.
    , bgroup "match many short" $ map (benchMatch genMany) sizes
    ]

sizes :: [Int]
sizes = [100, 10000, 500000]

benchBuild :: (Int -> RandStd [(C.ByteString, Int)]) -> Int -> Benchmark
benchBuild genps n = sizedBench n gen $ nf (fromTrieAC . fromListTAC) where
    gen = evalR $ genps n

benchMatch :: (Int -> RandStd [(C.ByteString, Int)]) -> Int -> Benchmark 
benchMatch genps n = sizedBench n gen $ \(ac, ps) -> nf (matchAC ac) ps where
    gen = evalR $
        (,) <$> (fromTrieAC . fromListTAC <$> genps n) <*> (C.pack <$> randLowerCaseString n)

genFew :: Int -> RandStd [(C.ByteString, Int)]
genFew n = do
    let n' = div n 20
    ps <- replicateM 20 $ C.pack <$> randLowerCaseString n'
    pure $ zip ps [1..]

genMany :: Int -> RandStd [(C.ByteString, Int)]
genMany n = do
    let n' = div n 20
    ps <- replicateM n' $ C.pack <$> randLowerCaseString 20
    pure $ zip ps [1..]
