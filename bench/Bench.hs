import Criterion.Main

import qualified BinSearchBench
import qualified FenwickBench
import qualified LCABench
import qualified PQTreeBench
import qualified PruferBench
import qualified SparseTableBench

main :: IO ()
main = defaultMain
    [ BinSearchBench.benchmark
    , FenwickBench.benchmark
    , LCABench.benchmark
    , PQTreeBench.benchmark
    , PruferBench.benchmark
    , SparseTableBench.benchmark
    ]
