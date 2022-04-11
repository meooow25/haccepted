{-# LANGUAGE ScopedTypeVariables #-}
module DijkstraSpec where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Graph

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Dijkstra ( Weight, dijkstra, dijkstraH )
import LabelledGraph ( LGraph, buildLG )

spec :: Spec
spec = do
    prop "dijkstra" $
        forAll genGraph $ \(g, srcs) -> do
            dijkstra g srcs `shouldBe` bellmanFord g srcs

    prop "dijkstraH" $
        forAll genGraph $ \(g, srcs) -> do
            dijkstraH g srcs `shouldBe` bellmanFord g srcs

genGraph :: Gen (LGraph Weight, [Vertex])
genGraph = sized $ \n -> do
    n' <- choose (0, n)
    l <- arbitrary
    let b = (l, l + n' - 1)
        edge = (,) <$> choose b <*> ((,) <$> (getNonNegative <$> arbitrary) <*> choose b)
    if n' == 0
        then pure (array b [], [])
        else (,) <$> (buildLG b <$> listOf edge) <*> listOf (choose b)

bellmanFord :: LGraph Weight -> [Vertex] -> UArray Vertex Weight
bellmanFord g srcs = runSTUArray $ do
    let bnds = bounds g
    d :: STUArray s Vertex Weight <- newArray bnds maxBound
    let relax (u, (w, v)) = do
            du <- readArray d u
            dv <- readArray d v
            let dv' = du + w
            when (du /= maxBound && dv' < dv) $
                writeArray d v dv' :: ST s ()
    forM_ srcs $ \v -> writeArray d v 0
    replicateM_ (rangeSize bnds) $ mapM_ relax [(u, wv) | (u, wvs) <- assocs g, wv <- wvs]
    pure d
