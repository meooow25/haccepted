import Criterion.Main

import qualified AhoCorasickBench
import qualified ArrayBench
import qualified BFSBench
import qualified BinSearchBench
import qualified CentroidDecompBench
import qualified ConvexHullBench
import qualified DinicBench
import qualified DSUBench
import qualified DijkstraBench
import qualified FenwickBench
import qualified FenwickMutBench
import qualified FloydWarshallBench
import qualified KMPBench
import qualified KruskalBench
import qualified LCABench
import qualified MathBench
import qualified MIntBench
import qualified MoBench
import qualified ModBench
import qualified PQTreeBench
import qualified PruferBench
import qualified SegTreeBench
import qualified SegTreeLazyBench
import qualified SegTreeLazyMutBench
import qualified SegTreeMutBench
import qualified SortBench
import qualified SparseTableBench
import qualified SuffixArrayBench
import qualified SuffixTreeBench
import qualified TwoSatBench
import qualified ZBench

main :: IO ()
main = defaultMain
    [ AhoCorasickBench.benchmark
    , ArrayBench.benchmark
    , BFSBench.benchmark
    , BinSearchBench.benchmark
    , CentroidDecompBench.benchmark
    , ConvexHullBench.benchmark
    , DinicBench.benchmark
    , DSUBench.benchmark
    , DijkstraBench.benchmark
    , FenwickBench.benchmark
    , FenwickMutBench.benchmark
    , FloydWarshallBench.benchmark
    , KMPBench.benchmark
    , KruskalBench.benchmark
    , LCABench.benchmark
    , MathBench.benchmark
    , MIntBench.benchmark
    , MoBench.benchmark
    , ModBench.benchmark
    , PQTreeBench.benchmark
    , PruferBench.benchmark
    , SegTreeBench.benchmark
    , SegTreeLazyBench.benchmark
    , SegTreeLazyMutBench.benchmark
    , SegTreeMutBench.benchmark
    , SortBench.benchmark
    , SparseTableBench.benchmark
    , SuffixArrayBench.benchmark
    , SuffixTreeBench.benchmark
    , TwoSatBench.benchmark
    , ZBench.benchmark
    ]
