module KMPSpec where

import Data.Array.Unboxed
import Data.List
import qualified Data.ByteString.Char8 as C

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import KMP ( failFunc, failFuncBS )

spec :: Spec
spec = do
    prop "failFunc binary" $
        forAll (arbitrary :: Gen [Bool]) $ \xs -> do
            let p = failFunc (length xs) (xs!!)
            bounds p `shouldBe` (0, length xs - 1)
            elems p  `shouldBe` naiveFailFunc xs
    prop "failFuncBS" $
        \(ASCIIString s) -> do
            let p = failFuncBS $ C.pack s
            bounds p `shouldBe` (0, length s - 1)
            elems p  `shouldBe` naiveFailFunc s

naiveFailFunc :: Eq a => [a] -> [Int]
naiveFailFunc xs = map getCnt $ tail xsInits where
    xsInits = inits xs
    ok cur chk = chk /= cur && chk `isSuffixOf` cur
    getCnt cur = length $ last $ filter (ok cur) xsInits
