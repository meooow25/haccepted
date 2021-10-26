import Criterion.Main

import qualified BFSBench
import qualified BinSearchBench
import qualified CentroidDecompBench
import qualified FenwickBench
import qualified KMPBench
import qualified LCABench
import qualified PQTreeBench
import qualified PruferBench
import qualified SparseTableBench
import qualified TwoSatBench
import qualified ZBench

main :: IO ()
main = defaultMain
    [ BFSBench.benchmark
    , BinSearchBench.benchmark
    , CentroidDecompBench.benchmark
    , FenwickBench.benchmark
    , KMPBench.benchmark
    , LCABench.benchmark
    , PQTreeBench.benchmark
    , PruferBench.benchmark
    , SparseTableBench.benchmark
    , TwoSatBench.benchmark
    , ZBench.benchmark
    ]
