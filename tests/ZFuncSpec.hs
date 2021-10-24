module ZFuncSpec where

import Data.Array.Unboxed
import Data.List
import qualified Data.ByteString.Char8 as C

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import ZFunc ( zFunc, zFuncBS )

spec :: Spec
spec = do
    prop "zFunc binary" $
        forAll (arbitrary :: Gen [Bool]) $ \xs -> do
            let z = zFunc (length xs) (xs!!)
            bounds z `shouldBe` (0, length xs - 1)
            elems z  `shouldBe` naiveZFunc xs
    prop "zFuncBS" $
        \(ASCIIString s) -> do
            let z = zFuncBS $ C.pack s
            bounds z `shouldBe` (0, length s - 1)
            elems z  `shouldBe` naiveZFunc s

naiveZFunc :: Eq a => [a] -> [Int]
naiveZFunc [] = []
naiveZFunc xs = 0 : map (length . takeWhile id . zipWith (==) xs) (tail $ init $ tails xs)
