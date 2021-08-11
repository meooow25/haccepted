{-|

MInt
Integer type for modular arithmetic, using fixed modulo
-}

module MInt where

import Data.Int
import Data.Ratio

m :: Int64
m = 1000000007
-- m = 998244353

newtype MInt = MInt {toInt64 :: Int64} deriving Show

instance Num MInt where
    MInt a + MInt b = MInt $ if c >= m then c - m else c where c = a + b
    MInt a - MInt b = MInt $ if c < 0 then c + m else c where c = a - b
    MInt a * MInt b = MInt $ a * b `mod` m
    abs             = id
    signum (MInt a) = MInt $ signum a
    fromInteger i   = MInt $ fromInteger i `mod` m

instance Fractional MInt where
    recip a        = a ^ (m - 2)
    fromRational r = MInt (fromIntegral $ numerator r) / MInt (fromIntegral $ denominator r)

inv :: MInt -> MInt
inv a = a ^ (m - 2)
