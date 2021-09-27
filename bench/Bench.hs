import Criterion.Main

import qualified BinSearchBench
import qualified FenwickBench
import qualified LCABench
import qualified PQTreeBench
import qualified PruferBench
import qualified SparseTableBench
import qualified TwoSatBench

main :: IO ()
main = defaultMain
    [ BinSearchBench.benchmark
    , FenwickBench.benchmark
    , LCABench.benchmark
    , PQTreeBench.benchmark
    , PruferBench.benchmark
    , SparseTableBench.benchmark
    , TwoSatBench.benchmark
    ]
