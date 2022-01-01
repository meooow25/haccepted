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
  does not reduce the tree to Nothing, there are at most two partial RNodes that float up to the
  root of the pertinent subtree.

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
            | PNode  [PQNode]
            | QNode  [PQNode]
            deriving Show

type Parts = ([PQNode], [PQNode]) -- empty and full, end first
data RNode = Empty PQNode
           | Full  PQNode
           | Part  Parts
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
mkPartial es fs = Part (es, fs)

emptyMany, fullMany :: State [RNode] [PQNode]
emptyMany = state $ go [] where
    go ys (Empty n : xs) = go (n:ys) xs
    go ys xs             = (reverse ys, xs)
fullMany = state $ go [] where
    go ys (Full n : xs) = go (n:ys) xs
    go ys xs            = (reverse ys, xs)

partialMaybe :: State [RNode] (Maybe Parts)
partialMaybe = state go where
    go (Part p : xs) = (Just p,  xs)
    go xs            = (Nothing, xs)

splitForP :: [RNode] -> ([PQNode], [Parts], [PQNode])
splitForP us = go us [] [] [] where
    go []     es ps fs = (es, ps, fs)
    go (x:xs) es ps fs = case x of
        Empty n  -> go xs (n:es) ps     fs
        Part p   -> go xs es     (p:ps) fs
        Full n   -> go xs es     ps     (n:fs)

splitForQ :: [RNode] -> Maybe ([PQNode], Maybe Parts, [PQNode])
splitForQ xs = go xs <|> go (reverse xs) where
    go = evalState $ go' <$> emptyMany <*> partialMaybe <*> fullMany <*> gets null
    go' es ps fs = ((es, ps, fs) <$) . guard

splitForQRoot :: [RNode] -> Maybe ([PQNode], Maybe Parts, [PQNode], Maybe Parts, [PQNode])
splitForQRoot = evalState $
    go <$> emptyMany <*> partialMaybe <*> fullMany <*> partialMaybe <*> emptyMany <*> gets null
  where
    go es1 ps1 fs ps2 es2 = ((es1, ps1, fs, ps2, es2) <$) . guard

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
        | ~[r] <- rts = Right $ f . getcs' <$> r
      where
        ys = map visit cs
        cnt = sum $ lefts ys
        rts = rights ys
        getcs' c' = zipWith (\c -> either (const c) (const c')) cs ys

    reduceRoot :: PQNode -> Maybe PQNode
    reduceRoot n@(PNode cs) = go . splitForP =<< mapM reduceInternal cs where
        go (es, ps, fs) = case ps of
            []       -> Just noPartial
            [p1]     -> Just $ withPartial p1 ([], [])
            [p1, p2] -> Just $ withPartial p1 p2
            _        -> Nothing
          where
            noPartial
                | null es || null fs = n
                | otherwise          = mkPNode $ mkPNode fs : es
            withPartial (pe1, pf1) (pe2, pf2) = mkPNode $ qn : es where
                qn = mkQNode $
                    pe1 ++ reverse pf1 ++ [mkPNode fs | not (null fs)] ++ pf2 ++ reverse pe2

    reduceRoot n@(QNode cs) = fmap go . splitForQRoot =<< mapM reduceInternal cs where
        go (es1, ps1, fs, ps2, es2) = case (ps1, ps2) of
            (Nothing, Nothing) -> n
            _                  -> withPartial (fromMaybe ([], []) ps1) (fromMaybe ([], []) ps2)
          where
            withPartial (pe1, pf1) (pe2, pf2) =
                mkQNode $ es1 ++ pe1 ++ reverse pf1 ++ fs ++ pf2 ++ reverse pe2 ++ es2

    reduceRoot n@(PQLeaf _) = Just n

    reduceInternal :: PQNode -> Maybe RNode
    reduceInternal n@(PNode cs) = go . splitForP =<< mapM reduceInternal cs where
        go (es, ps, fs) = case ps of
            []   -> Just noPartial
            [p1] -> Just $ withPartial p1
            _    -> Nothing
          where
            noPartial
                | null es   = Full n
                | null fs   = Empty n
                | otherwise = mkPartial [mkPNode es] [mkPNode fs]
            withPartial (pe, pf) = mkPartial
                ([mkPNode es | not (null es)] ++ pe) ([mkPNode fs | not (null fs)] ++ pf)

    reduceInternal n@(QNode cs) = fmap go . splitForQ =<< mapM reduceInternal cs where
        go (es, ps, fs) = maybe noPartial withPartial ps where
            noPartial
                | null es   = Full n
                | null fs   = Empty n
                | otherwise = mkPartial es (reverse fs)
            withPartial (pe, pf) = mkPartial (es ++ pe) (reverse fs ++ pf)

    reduceInternal n@(PQLeaf x) = Just $ if x `IS.member` xs then Full n else Empty n

reduceAllPQ :: [[Int]] -> PQNode -> Maybe PQNode
reduceAllPQ xss t = foldM (flip reducePQ) t xss

frontierPQ :: PQNode -> [Int]
frontierPQ = flip go [] where
    go (PQLeaf x) acc = x:acc
    go (PNode cs) acc = foldr go acc cs
    go (QNode cs) acc = foldr go acc cs

permsPQ :: PQNode -> [[Int]]
permsPQ n = case n of
    PQLeaf x -> [[x]]
    PNode cs -> concatMap prod $ permutations $ map permsPQ cs
    QNode cs -> prod (map permsPQ cs) ++ prod (reverse $ map permsPQ cs)
  where
    prod = map concat . sequence

-- Note: permsPQ is far from optimal, but it is unlikely to be used with larger values of n anyway

--------------------------------------------------------------------------------
-- For tests

instance NFData PQNode where
    rnf (PQLeaf x) = rnf x
    rnf (PNode cs) = rnf cs
    rnf (QNode cs) = rnf cs
