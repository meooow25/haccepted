{-
PQ-tree, a data structure for representing permutations

An implementation of PQ-tree as described by Booth and Lueker, useful for solving the consecutive
ones problem. The tree consists of three types of nodes, P, Q and leaf. Leaves are single elements.
The frontier of the tree is the left to right order of leaves in the tree. A P-node is equivalent to
another with its children permuted in any order. A Q-node is equivalent to one with its children
reversed. The permutations represented by a tree are the frontiers of all equivalent trees.

The various reduction templates described in Booth and Lueker's paper are used, but the construction
algorithm is not. This implementation is purely functional, hence it uses a simpler top-down
approach instead that runs in O(n) time (plus set member check overheads) rather than in
O(update size).
This implementation is specialized to Ints, but can be modified to work with other types.

Sources:
* https://en.wikipedia.org/wiki/PQ_tree
* Kellogg S. Booth and George S. Lueker, "Testing for the consecutive ones property, interval
  graphs, and graph planarity using PQ-tree algorithms", 1976
  https://www.sciencedirect.com/science/article/pii/S0022000076800451

Implementation notes:
* In reduce, the root of the pertinent subtree is found using pertinent node counts, then another
  pass is made through the pertinent subtree performing the actual reduction.
* The RNode type is a temporary node indicating empty, full, or partial status. In a reduction that
  does not reduce the tree to Nothing, there are at most two RNodes that float up to the root
  of the pertinent subtree.

buildPQ
Builds a PQ-tree from a set of Ints. O(n).

reducePQ
Reduces a PQ-tree with a set of Ints. This should be a subset of what it was constructed with. O(n).

reduceAllPQ
Reduces a PQ-tree with many sets. O(mn) for m sets.

frontierPQ
The frontier of the PQ-tree. O(n).

permsPQ
The permutations represented by the PQ-tree. O(do not use).
-}

module PQTree
    ( PQNode
    , buildPQ
    , reducePQ
    , reduceAllPQ
    , frontierPQ
    , permsPQ
    ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.State
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.IntSet as IS

data PQNode = PQLeaf !Int
            | PNode [PQNode]
            | QNode [PQNode]
            deriving Show

data RNode = Em { unRNode :: PQNode }
           | Fu { unRNode :: PQNode }
           | Pa [PQNode] [PQNode] -- empty and full, end first
           deriving Show

mkPNode, mkQNode :: [PQNode] -> PQNode
mkPNode []     = error "empty PNode"
mkPNode [n]    = n
mkPNode ns     = PNode ns
mkQNode []     = error "empty QNode"
mkQNode [n]    = n
mkQNode [n, m] = PNode [n, m]
mkQNode ns     = QNode ns

mkPartial :: [PQNode] -> [PQNode] -> RNode
mkPartial [] _  = error "empty RNode es"
mkPartial _  [] = error "empty RNode fs"
mkPartial es fs = Pa es fs

isFu, isEm, isPa :: RNode -> Bool
isFu (Fu _)   = True
isFu _        = False
isEm (Em _)   = True
isEm _        = False
isPa (Pa _ _) = True
isPa _        = False

splitForP :: [RNode] -> ([PQNode], [RNode], [PQNode])
splitForP us = go us [] [] [] where
    go []     es ps fs = (es, ps, fs)
    go (x:xs) es ps fs = case x of
        Em n   -> go xs (n:es) ps     fs
        Pa _ _ -> go xs es     (x:ps) fs
        Fu n   -> go xs es     ps     (n:fs)

span1 :: (a -> Bool) -> [a] -> ([a], [a])
span1 _ [] = ([], [])
span1 f xs@(x:xs')
    | f x       = ([x], xs')
    | otherwise = ([],  xs)

splitForQ :: [RNode] -> Maybe ([PQNode], [RNode], [PQNode])
splitForQ xs = go xs <|> go (reverse xs) where
    go = evalState $ do
        ~[es, ps, fs] <- mapM state [span isEm, span1 isPa, span isFu]
        gets $ ((map unRNode es, ps, map unRNode fs) <$) . guard . null

splitForQRoot :: [RNode] -> Maybe ([PQNode], [RNode], [PQNode], [RNode], [PQNode])
splitForQRoot = evalState $ do
    ~[e1, p1, fs, p2, e2] <- mapM state [span isEm, span1 isPa, span isFu, span1 isPa, span isEm]
    gets $ ((map unRNode e1, p1, map unRNode fs, p2, map unRNode e2) <$) . guard . null

buildPQ :: [Int] -> PQNode
buildPQ = mkPNode . map PQLeaf

reducePQ :: [Int] -> PQNode -> Maybe PQNode
reducePQ []  = Just
reducePQ xs0 = fromRight (error "outside initial set") . visit where
    xs = IS.fromList xs0
    xsz = IS.size xs

    visit :: PQNode -> Either Int (Maybe PQNode)
    visit n@(PNode cs) = visitPQ n PNode cs
    visit n@(QNode cs) = visitPQ n QNode cs
    visit n@(PQLeaf x)
        | x `IS.notMember` xs = Left 0
        | xsz > 1             = Left 1
        | otherwise           = Right $ reduceRoot n
    visitPQ n f cs
        | cnt == xsz  = Right $ reduceRoot n
        | null rts    = Left cnt
        | ~[r] <- rts = Right $ f cs' <$ r
      where
        ys = map visit cs
        cnt = sum $ lefts ys
        rts = rights ys
        cs' = zipWith (\c e -> either (const c) fromJust e) cs ys

    reduceRoot :: PQNode -> Maybe PQNode
    reduceRoot = go where
        go n@(PNode cs) = do
            (es, ps, fs) <- splitForP <$> mapM reduceInternal cs
            let noPartial
                    | null es || null fs = n
                    | otherwise          = mkPNode $ mkPNode fs : es
                withPartial pe1 pf1 pe2 pf2 = mkPNode $ qn : es where
                    qn = mkQNode $
                        pe1 ++ reverse pf1 ++ [mkPNode fs | not (null fs)] ++ pf2 ++ reverse pe2
            case ps of
                []                       -> Just noPartial
                [Pa pe1 pf1]             -> Just $ withPartial pe1 pf1 []  []
                [Pa pe1 pf1, Pa pe2 pf2] -> Just $ withPartial pe1 pf1 pe2 pf2
                _                        -> Nothing

        go n@(QNode cs) = do
            (e1, p1, fs, p2, e2) <- splitForQRoot =<< mapM reduceInternal cs
            let withPartial pe1 pf1 pe2 pf2 =
                    mkQNode $ e1 ++ pe1 ++ reverse pf1 ++ fs ++ pf2 ++ reverse pe2 ++ e2
            case (p1, p2) of
                ([],           [])           -> Just n
                ([Pa pe1 pf1], [])           -> Just $ withPartial pe1 pf1 []  []
                ([],           [Pa pe2 pf2]) -> Just $ withPartial []  []  pe2 pf2
                ([Pa pe1 pf1], [Pa pe2 pf2]) -> Just $ withPartial pe1 pf1 pe2 pf2
                _                            -> Nothing

        go n@(PQLeaf _) = Just n

    reduceInternal :: PQNode -> Maybe RNode
    reduceInternal = go where
        go n@(PNode cs) = do
            (es, ps, fs) <- splitForP <$> mapM go cs
            let noPartial
                    | null es   = Fu n
                    | null fs   = Em n
                    | otherwise = mkPartial [mkPNode es] [mkPNode fs]
                withPartial pe pf = mkPartial
                    ([mkPNode es | not (null es)] ++ pe) ([mkPNode fs | not (null fs)] ++ pf)
            case ps of
                []         -> Just noPartial
                [Pa pe pf] -> Just $ withPartial pe pf
                _          -> Nothing

        go n@(QNode cs) = do
            (es, ps, fs) <- splitForQ =<< mapM go cs
            let noPartial
                    | null es   = Fu n
                    | null fs   = Em n
                    | otherwise = mkPartial es (reverse fs)
                withPartial pe pf = mkPartial (es ++ pe) (reverse fs ++ pf)
            case ps of
                []         -> Just noPartial
                [Pa pe pf] -> Just $ withPartial pe pf
                _          -> Nothing

        go n@(PQLeaf x) = Just $ if x `IS.member` xs then Fu n else Em n

reduceAllPQ :: [[Int]] -> PQNode -> Maybe PQNode
reduceAllPQ xss t = foldM (flip reducePQ) t xss

frontierPQ :: PQNode -> [Int]
frontierPQ = flip go [] where
    go (PQLeaf x) acc = x:acc
    go (PNode cs) acc = foldr go acc cs
    go (QNode cs) acc = foldr go acc cs

permsPQ :: PQNode -> [[Int]]
permsPQ (PQLeaf x) = [[x]]
permsPQ n
    | PNode cs <- n = concatMap prod $ permutations $ map permsPQ cs
    | QNode cs <- n = prod (map permsPQ cs) ++ prod (reverse $ map permsPQ cs)
    where prod xs = concat <$> sequence xs

-- Note: permsPQ is far from optimal, but it is unlikely to be used with larger values of n anyway

--------------------------------------------------------------------------------
-- For tests

instance NFData PQNode where
    rnf (PQLeaf x) = rnf x
    rnf (PNode cs) = rnf cs
    rnf (QNode cs) = rnf cs
