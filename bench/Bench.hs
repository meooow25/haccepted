import Criterion.Main

import qualified BinSearchBench
import qualified FenwickBench

main :: IO ()
main = defaultMain [
        BinSearchBench.benchmark
      , FenwickBench.benchmark
    ]
