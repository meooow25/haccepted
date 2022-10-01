module ZFuncSpec where

import Data.Array.Unboxed
import Data.List
import qualified Data.ByteString.Char8 as C

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import ZFunc ( zFunc, zFuncBS )
import Util ( genASCIIBS )

spec :: Spec
spec = do
    prop "zFunc binary" $
        forAll (arbitrary :: Gen [Bool]) $ \xs -> do
            let z = zFunc (length xs) (xs!!)
            bounds z `shouldBe` (0, length xs - 1)
            elems z  `shouldBe` naiveZFunc xs
    prop "zFuncBS" $
        forAll genASCIIBS $ \s -> do
            let z = zFuncBS s
            bounds z `shouldBe` (0, C.length s - 1)
            elems z  `shouldBe` naiveZFunc (C.unpack s)

naiveZFunc :: Eq a => [a] -> [Int]
naiveZFunc [] = []
naiveZFunc xs = 0 : map (length . takeWhile id . zipWith (==) xs) (tail $ init $ tails xs)
