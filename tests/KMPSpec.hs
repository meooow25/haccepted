module KMPSpec where

import Data.Array.Unboxed
import Data.List
import qualified Data.ByteString.Char8 as C

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import KMP ( prefixFunc, prefixFuncBS )
import Util ( genASCIIBS )

spec :: Spec
spec = do
    prop "prefixFunc binary" $
        forAll (arbitrary :: Gen [Bool]) $ \xs -> do
            let p = prefixFunc (length xs) (xs!!)
            bounds p `shouldBe` (0, length xs - 1)
            elems p  `shouldBe` naivePrefixFunc xs
    prop "prefixFuncBS" $
        forAll genASCIIBS $ \s -> do
            let p = prefixFuncBS s
            bounds p `shouldBe` (0, C.length s - 1)
            elems p  `shouldBe` naivePrefixFunc (C.unpack s)

naivePrefixFunc :: Eq a => [a] -> [Int]
naivePrefixFunc xs = map getCnt $ tail xsInits where
    xsInits = inits xs
    ok cur chk = chk /= cur && chk `isSuffixOf` cur
    getCnt cur = length $ last $ filter (ok cur) xsInits
