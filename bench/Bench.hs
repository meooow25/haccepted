import Criterion.Main

import qualified AhoCorasickBench
import qualified BFSBench
import qualified BinSearchBench
import qualified CentroidDecompBench
import qualified ConvexHullBench
import qualified DinicBench
import qualified DSUBench
import qualified DijkstraBench
import qualified FenwickBench
import qualified FloydWarshallBench
import qualified KMPBench
import qualified KruskalBench
import qualified LCABench
import qualified MoBench
import qualified PQTreeBench
import qualified PruferBench
import qualified SegTreeBench
import qualified SegTreeLazyBench
import qualified SortBench
import qualified SparseTableBench
import qualified TwoSatBench
import qualified ZBench

main :: IO ()
main = defaultMain
    [ AhoCorasickBench.benchmark
    , BFSBench.benchmark
    , BinSearchBench.benchmark
    , CentroidDecompBench.benchmark
    , ConvexHullBench.benchmark
    , DinicBench.benchmark
    , DSUBench.benchmark
    , DijkstraBench.benchmark
    , FenwickBench.benchmark
    , FloydWarshallBench.benchmark
    , KMPBench.benchmark
    , KruskalBench.benchmark
    , LCABench.benchmark
    , MoBench.benchmark
    , PQTreeBench.benchmark
    , PruferBench.benchmark
    , SegTreeBench.benchmark
    , SegTreeLazyBench.benchmark
    , SortBench.benchmark
    , SparseTableBench.benchmark
    , TwoSatBench.benchmark
    , ZBench.benchmark
    ]
