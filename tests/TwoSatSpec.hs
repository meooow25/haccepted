module TwoSatSpec where

import Data.Array
import Data.Maybe

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import TwoSat ( Var(..), solve2Sat )

spec :: Spec
spec = do
    prop "solve2Sat" $
        forAll genxys $ \(bnds, xys) -> do
            let sol = solve2Sat bnds xys
            label (if isJust sol then "has sol" else "no sol") $
                sol `shouldSatisfy` validate bnds xys

genxys :: Gen ((Int, Int), [(Var, Var)])
genxys = sized $ \n' -> do
    n <- choose (0, n' `min` 15)
    l <- arbitrary
    let r = l + n - 1
        genVar = do
            b <- arbitrary
            x <- choose (l, r)
            pure $ if b then Id x else Not x
    xys <- if n == 0
        then pure []
        else listOf $ (,) <$> genVar <*> genVar
    pure ((l, r), xys)

validate :: (Int, Int) -> [(Var, Var)] -> Maybe [Bool] -> Bool
validate bnds xys sol = case sol of
    Just sol' -> checkSol sol'
    Nothing   -> not $ any checkSol allOptions
  where
    allOptions = mapM (const [False, True]) $ range bnds
    checkSol sol = all ok xys where
        a = listArray bnds sol
        get (Id x)  = a!x
        get (Not x) = not $ a!x
        ok (x, y) = get x || get y
