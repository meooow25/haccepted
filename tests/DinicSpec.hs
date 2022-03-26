{-# LANGUAGE ScopedTypeVariables #-}
module DinicSpec where

import Control.Exception
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.Graph

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Dinic ( Flow, FlowEdge(..), FlowResult(..), ToEdge(..), dinic )
import Misc ( modifyArray )

spec :: Spec
spec = do
    prop "random graph" $
        forAll genFlowG $ \(bnds, es, src, sink) -> do
            let FlowResult flow flows minCut = dinic bnds es src sink
            flow `shouldBe` fordFulkerson bnds es src sink
            zip es flows `shouldSatisfy` all (\(FlowEdge _ _ c, f) -> f <= c)
            zip3 es flows minCut `shouldSatisfy` all (\(FlowEdge _ _ c, f, b) -> not b || f == c)
            sum [if b then f else 0 | (f, b) <- zip flows minCut] `shouldBe` flow
            let gAfterCut = buildG bnds [(u, v) | (FlowEdge u v _, b) <- zip es minCut, not b]
            path gAfterCut src sink `shouldSatisfy` not

    it "src == sink" $
        evaluate (dinic (1, 1) [] 1 1) `shouldThrow` errorCall "src == sink"

genFlowG :: Gen (Bounds, [FlowEdge], Vertex, Vertex)
genFlowG = do
    n <- getSize `suchThat` (>=2)
    n' <- choose (2, n)
    l <- arbitrary
    let b = (l, l + n' - 1)
    es <- listOf $ FlowEdge <$> choose b <*> choose b <*> (getNonNegative <$> arbitrary)
    src <- choose b
    sink <- choose b `suchThat` (/=src)
    pure (b, es, src, sink)

fordFulkerson :: Bounds -> [FlowEdge] -> Vertex -> Vertex -> Flow
fordFulkerson bnds es src sink = runST $ do
    flow :: STUArray s Int Flow <- newListArray (0, 2*m-1) $ concat [[0, c] | FlowEdge _ _ c <- es]
    let runFF = do
            vis :: STUArray s Vertex Bool <- newArray bnds False
            let dfs _ 0 = pure 0 :: ST s Flow
                dfs u fup | u == sink = pure fup
                dfs u fup = do
                    done <- readArray vis u
                    if done
                        then pure 0
                        else writeArray vis u True >> foldr tryEdge (pure 0) (g!u)
                  where
                    tryEdge (ToEdge v i) other = do
                        f <- readArray flow (xor i 1)
                        fdn <- dfs v (min fup f)
                        if fdn == 0 then other else do
                            modifyArray flow i (+fdn) 
                            modifyArray flow (xor i 1) (+(-fdn))
                            pure fdn
            f <- dfs src maxBound
            if f == 0 then pure 0 else (f+) <$> runFF
    runFF
  where
    m = length es
    g :: Array Vertex [ToEdge] = accumArray (flip (:)) [] bnds $ do
        (i, FlowEdge u v _) <- zip [0..] es
        [(u, ToEdge v (2*i)), (v, ToEdge u (2*i+1))]
