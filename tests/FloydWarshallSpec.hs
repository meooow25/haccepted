module FloydWarshallSpec where

import Data.Graph
import Data.Semigroup

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import FloydWarshall ( WEdge(..), Weight, floydWarshallFromEdges )

spec :: Spec
spec = do
    prop "random graph" $
        forAll genData $ \(b, es, u, v) -> do
            floydWarshallFromEdges b es u v `shouldBe` naiveShortestPath es u v

    it "example graph" $ do
        let qry = floydWarshallFromEdges (1, 4) exampleGraph
        [((i, j), qry i j) | i <- [1..4], j <- [1..4]] `shouldBe` exampleGraphShortestPaths

genData :: Gen (Bounds, [WEdge], Vertex, Vertex)
genData = sized $ \n -> do
    n' <- choose (1, max 1 $ min 5 n)
    l <- arbitrary
    let b = (l, l + n' - 1)
        genWEdge = do
            u <- choose b
            v <- choose b
            NonNegative w <- arbitrary
            pure $ WEdge u v w
    (,,,) b <$> listOf genWEdge <*> choose b <*> choose b

naiveShortestPath :: [WEdge] -> Vertex -> Vertex -> Maybe Weight
naiveShortestPath es u v = getMin <$> go u [] where
    go u _ | u == v = Just (Min 0)
    go u vis = mconcat [fmap (w+) <$> go v (u:vis) | WEdge u' v w <- es, u == u', v `notElem` vis]

-- Has negative edges
exampleGraph :: [WEdge]
exampleGraph =
    [ WEdge 1 2 5
    , WEdge 1 3 (-3)
    , WEdge 3 4 1
    , WEdge 4 1 4
    ]

exampleGraphShortestPaths :: [((Vertex, Vertex), Maybe Weight)]
exampleGraphShortestPaths =
    [ ((1, 1), Just 0), ((1, 2), Just 5), ((1, 3), Just (-3)), ((1, 4), Just (-2))
    , ((2, 1), Nothing), ((2, 2), Just 0), ((2, 3), Nothing), ((2, 4), Nothing)
    , ((3, 1), Just 5), ((3, 2), Just 10), ((3, 3), Just 0), ((3, 4), Just 1)
    , ((4, 1), Just 4), ((4, 2), Just 9), ((4, 3), Just 1), ((4, 4), Just 0)
    ]
