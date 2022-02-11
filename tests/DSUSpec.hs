module DSUSpec where

import Data.Array.IO
import Data.List
import Data.Maybe

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

import DSU ( newD, sameSetD, unionD )
import Util ( genIntPair )

spec :: Spec
spec = do
    prop "unions then find" $
        forAll genDSUData $ \dsuData -> monadicIO $ do
            res <- run $ runDSU dsuData
            assert $ res == runSimpleDSU dsuData

type DSUData = ((Int, Int), [(Int, Int)], (Int, Int)) -- bounds, unions, find

genDSUData :: Gen DSUData
genDSUData = do
    n <- getSize `suchThat` (>=1)
    n' <- choose (1, n)
    NonNegative l <- arbitrary
    let bnds = (l, l + n' - 1)
    ijs <- listOf $ genIntPair bnds
    ij <- genIntPair bnds
    pure (bnds, ijs, ij)

runDSU :: DSUData -> IO ([Bool], Bool)
runDSU (bnds, ijs, (i, j)) = do
    dsu <- newD bnds :: IO (IOUArray Int Int)
    bs <- mapM (uncurry (unionD dsu)) ijs
    b <- sameSetD dsu i j
    pure (bs, b)

type SimpleDSU = [[Int]]

sameSimple :: Int -> Int -> SimpleDSU -> Bool
sameSimple i j xss = j `elem` fromJust (find (elem i) xss)

unionSimple :: Int -> Int -> SimpleDSU -> (SimpleDSU, Bool)
unionSimple i j xss
    | sameSimple i j xss = (xss, False)
    | otherwise          = ((xsi ++ xsj) : xss'', True)
  where
    (xsi, xss')  = remove (elem i) xss
    (xsj, xss'') = remove (elem j) xss'

remove :: (a -> Bool) -> [a] -> (a, [a])
remove f = go where
    go [] = error "not found"
    go (x:xs)
        | f x       = (x, xs)
        | otherwise = (x:) <$> go xs

runSimpleDSU :: DSUData -> ([Bool], Bool)
runSimpleDSU (bnds, ijs, (i, j)) = (bs, b) where
    dsu = map (:[]) (range bnds)
    (dsu', bs) = mapAccumL (flip (uncurry unionSimple)) dsu ijs
    b = sameSimple i j dsu'
