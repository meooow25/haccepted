import Criterion.Main

import qualified BFSBench
import qualified BinSearchBench
import qualified CentroidDecompBench
import qualified FenwickBench
import qualified KMPBench
import qualified LCABench
import qualified PQTreeBench
import qualified PruferBench
import qualified SegTreeBench
import qualified SegTreeLazyBench
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
    , SegTreeBench.benchmark
    , SegTreeLazyBench.benchmark
    , SparseTableBench.benchmark
    , TwoSatBench.benchmark
    , ZBench.benchmark
    ]
