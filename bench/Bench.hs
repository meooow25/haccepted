import Criterion.Main

import qualified BinSearchBench
import qualified FenwickBench
import qualified SparseTableBench

main :: IO ()
main = defaultMain
    [ BinSearchBench.benchmark
    , FenwickBench.benchmark
    , SparseTableBench.benchmark
    ]
