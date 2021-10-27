module BFSSpec where

import Data.Graph

import Test.Hspec

import BFS ( bfs )

spec :: Spec
spec = do
    it "bfs emptyGraph" $
        bfs emptyGraph [] `shouldBe` []
    it "bfs smallCompleteGraph single source" $
        bfs smallCompleteGraph [1] `shouldBe` [Node 1 [Node 2 [], Node 3 []]]
    it "bfs smallCompleteGraph multi-source" $
        bfs smallCompleteGraph [2, 3] `shouldBe` [Node 2 [Node 1 []], Node 3 []]

emptyGraph :: Graph
emptyGraph = buildG (0, -1) []

smallCompleteGraph :: Graph
smallCompleteGraph = buildG (1, 3)
    [ (1, 2), (2, 1)
    , (1, 3), (3, 1)
    , (2, 3), (3, 2)
    ]
