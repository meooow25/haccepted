{-
2-satisfiability

Solves the 2-sat problem, which assigns boolean values to variables (a, b, c...) such that it
satisfies a boolean expression which is a conjunction of clauses, where each clause is a disjunction
of two variables. For example, (a || b) && (a || not c) && (not b || not c).
The solution is obtained from the strongly connected components of the implication graph constructed
from the clauses.

Sources:
* https://en.wikipedia.org/wiki/2-satisfiability
* https://cp-algorithms.com/graph/2SAT.html

solve2Sat
Solves the 2-sat problem where each variable is represented an Int in the range (l, r). Returns a
list of assignments for the variables l to r, in order. O(n + m) where n = r - l + 1 and
m = length xys.
-}

module TwoSat
    ( Var(..)
    , solve2Sat
    ) where

import Control.DeepSeq
import Data.Array
import Data.Bits
import Data.Foldable
import Data.Graph

data Var = Id !Int | Not !Int deriving Show

solve2Sat :: (Int, Int) -> [(Var, Var)] -> Maybe [Bool]
solve2Sat (l, r) xys = assign $ elems comp where
    n = r - l + 1
    bnds = (0, 2 * n - 1)
    idx (Id x)  = (x - l) * 2
    idx (Not x) = (x - l) * 2 + 1
    g = buildG bnds $ do
        (x, y) <- xys
        [(idx x `xor` 1, idx y), (idx y `xor` 1, idx x)]
    comp = array bnds $ concat $ zipWith zip (map toList $ scc g) (map repeat [1 :: Int ..])
    assign [] = Just []
    assign ~(cx:cnotx:rest)
        | cx == cnotx = Nothing
        | otherwise   = ((cx < cnotx):) <$> assign rest

--------------------------------------------------------------------------------
-- For tests

instance NFData Var where
    rnf (Id x)  = rnf x
    rnf (Not x) = rnf x
