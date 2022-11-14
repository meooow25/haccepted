{-# LANGUAGE ScopedTypeVariables #-}
{-|
Suffix tree

A moderately simple and flexible suffix tree.
A suffix tree of a string is a compressed trie of all the suffixes of the string, useful for fast
matching on substrings of the string or to calculate certain properties of the string.
Other suffix structures such as suffix arrays and suffix automata may serve as alternates to suffix
trees.

The implementation here constructs a SuffixTree a from a given string index function, with string
elements of type Int. Ukkonen's algorithm is used for construction. This is not lazy, the entire
tree is constructed right away.
Only the implicit suffix tree is constructed, which is a suffix tree where suffixes that are
prefixes of other suffixes do not end in leaves. To make them end in leaves, set the last element
of the string to a unique value.
A SuffixTree a also stores accumulated values of type a at every node. These values are calculated
using user supplied functions, which can be chosen based on what the tree will be used for.

Sources:
* Esko Ukkonen, "On–line construction of suffix trees", 1995
  https://www.cs.helsinki.fi/u/ukkonen/SuffixT1withFigs.pdf
* Dan Gusfield, "Algorithms on Strings, Trees, and Sequences", 1997
  https://doi.org/10.1017/CBO9780511574931

Implementation notes:
* Ukkonen's algorithm is used to construct the suffix tree. The implementation here is similar to
  the one in his paper. The state maintained is (u, pos). For index i the substring [pos..i] must be
  inserted at node u, then at the suffix link of u, and so on.
* buildSufT builds a nicer-to-use tree on top of arrays calculated by Ukkonen's algorithm.
* Suffix links, which can be useful in some situations, are not retained for simplicity.
* There is no setup for fast LCA queries, which are also useful in some situations.
* matchSufT can be easily modified to return the value for the longest prefix of the pattern
  matched, if it does not match completely.

String indexing is assumed to take O(1). Let k be the alphabet size. Let the complexity of IntMap
operations be f(n), where n is the size of the map. f(n) is O(min(n, word size)), see IntMap
documentation for details.

buildSufT
Builds a suffix tree. n is the length of the string. at is a 0-based indexing function into the
string. fromLeaf constructs a value from a leaf index. updEdge constructs a value from an existing
value and a length of an edge leading to it. merge combines two values.
Examples:
  To query the number of times a pattern occurs in the string, use const 1, const, and (+).
  To calculate the number of distinct substrings, use const 0, (+), and (+).
O(n * f(k)), plus the above functions are each called O(n) times.

matchSufT
Matches a pattern on the suffix tree. updEdge and at are as defined for buildSufT. m is the length
of the pattern. at' is a 0-based indexing function into the pattern. If the pattern is not present,
returns Nothing. Otherwise returns the accumulated value for the subtree where the pattern ends.
O(m * f(k)) plus at most one call of updEdge.

buildMatchSufT
buildSufT together with matchSufT to avoid having to repeat arguments. Apply partially for
multiple queries.

drawSufT
Draws a suffix tree. Can be used for debugging.
-}

module SuffixTree
    ( SufTNode(..)
    , SufTEdge(..)
    , Chr
    , buildSufT
    , matchSufT
    , buildMatchSufT
    , drawSufT
    ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.List
import qualified Data.IntMap.Strict as IM

import TreeDraw ( draw )

type Chr = Int

data SufTNode a = SufTNode !a !(IM.IntMap (SufTEdge a))
data SufTEdge a = SufTEdge !Int !Int !(SufTNode a)

buildSufT :: (Int -> a) -> (a -> Int -> a) -> (a -> a -> a)
          -> Int -> (Int -> Chr)
          -> SufTNode a
buildSufT fromLeaf updEdge merge n at = mkNode n 0 where
    (nxta, lefta, lena) = ukkonen n at
    mkNode dep i = SufTNode a nxt where
        nxt = IM.map (\j -> SufTEdge (lefta!j) (lena!j) (mkNode (dep - lena!j) j)) (nxta!i)
        a | IM.null nxt = fromLeaf dep
          | otherwise   = foldl1' merge [updEdge a' len | SufTEdge _ len (SufTNode a' _) <- IM.elems nxt]

ukkonen :: Int -> (Int -> Chr) -> (Array Int (IM.IntMap Int), UArray Int Int, UArray Int Int)
ukkonen n at = runST $ do
    let sz = max (n + 1) (2 * n - 1)
    nxt :: STArray s Int (IM.IntMap Int) <- newArray (0, sz - 1) IM.empty
    [suf, left, len] :: [STUArray s Int Int] <- replicateM 3 (newArray (0, sz - 1) 0)
    cur :: STUArray s () Int <- newArray ((), ()) 1
    let root = 0
        nxtId = readArray cur () >>= \i -> i <$ writeArray cur () (i + 1)
        step i = go root where
            go prv pos u = do
                nxtu <- readArray nxt u
                case nxtu IM.!? at pos of
                    Nothing -> insLeafGo u
                    Just v  -> join $ tryEdge nxtu v <$> readArray left v <*> readArray len v
              where
                tryEdge nxtu v leftv lenv
                    | pos + lenv <= i = go prv (pos + lenv) v
                    | at i /= at j    = doSplit >>= insLeafGo
                    | pos == leftv    = goSuf u
                    | otherwise       = (u, pos) <$ setSufPrv u
                  where
                    lenw = i - pos
                    j = leftv + lenw
                    doSplit = do
                        w <- nxtId
                        writeArray left w leftv
                        writeArray len w lenw
                        writeArray nxt w $! IM.singleton (at j) v
                        writeArray left v (leftv + lenw)
                        writeArray len v (lenv - lenw)
                        writeArray nxt u $! IM.insert (at pos) w nxtu
                        pure w
                insLeafGo v = setSufPrv v *> insLeaf v *> goSuf v
                setSufPrv v = writeArray suf prv v :: ST s ()
                insLeaf v = do
                    w <- nxtId
                    writeArray left w i
                    writeArray len w (n - i)
                    nxtv <- readArray nxt v
                    writeArray nxt v $! IM.insert (at i) w nxtv
                goSuf v
                    | u /= root = readArray suf u >>= go v pos
                    | pos < i   = go v (pos + 1) root
                    | otherwise = pure (root, pos)
    foldM_ (\(u, pos) i -> step i pos u) (root, 0) [0 .. n-1]
    (,,) <$> unsafeFreeze nxt <*> unsafeFreeze left <*> unsafeFreeze len

matchSufT :: (a -> Int -> a) -> (Int -> Chr) -> SufTNode a
          -> Int -> (Int -> Chr)
          -> Maybe a
matchSufT updEdge at (SufTNode a nxt) m at' = if m == 0 then Just a else go nxt 0 where
    go nxt i = IM.lookup (at' i) nxt >>= go' where
        go' (SufTEdge left len (SufTNode a nxt'))
            | i' == m && d == len = Just a
            | i' == m             = Just (updEdge a (len - d))
            | d == len            = go nxt' i'
            | otherwise           = Nothing
          where
            d = commonPrefix (min len (m - i)) (at . (+left)) (at' . (+i))
            i' = i + d
    commonPrefix n f g = until (\i -> i == n || f i /= g i) (+1) 0

buildMatchSufT :: (Int -> a) -> (a -> Int -> a) -> (a -> a -> a)
               -> Int -> (Int -> Chr)
               -> Int -> (Int -> Chr)
               -> Maybe a
buildMatchSufT fromLeaf updEdge merge n at = matchSufT updEdge at st where
    st = buildSufT fromLeaf updEdge merge n at

--------------------------------------------------------------------------------
-- For tests

instance NFData a => NFData (SufTEdge a) where
    rnf (SufTEdge _ _ u) = rnf u

instance NFData a => NFData (SufTNode a) where
    rnf (SufTNode a nxt) = rnf a `seq` rnf nxt

drawSufT :: Show a => SufTNode a -> String
drawSufT = draw showa nbs where
    showa (SufTNode a _) = show a
    nbs (SufTNode _ nxt) = [(Just (show (left, len)), v)| SufTEdge left len v <- IM.elems nxt]
