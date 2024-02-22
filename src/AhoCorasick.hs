{-# LANGUAGE BangPatterns #-}

{-|
Aho-Corasick algorithm

The Aho-Corasick algorithm builds an automaton from a set of pattern strings, and then uses it to
find positions in a search string where each of the pattern strings occur.

This implementation only works on ByteStrings, to keep things fast. If required it can be adapted
to work on other sequence types.

A TrieAC a can be constructed from pattern strings with associated values a, which can be then be
turned into an ACRoot a. An ACRoot a can then be run on a search string to find matches.
Construction and matching are both lazy.

Sources:
* Alfred V. Aho and Margaret J. Corasick, "Efficient string matching: An aid to bibliographic
  search", 1975
  https://dl.acm.org/doi/10.1145/360825.360855
* Stanford CS166 Aho-Corasick lecture slides
  https://web.stanford.edu/class/archive/cs/cs166/cs166.1166/lectures/04/Slides04.pdf

Implementation notes:
* We have to be lazy in the (Maybe (ACNode a)) and the [a] in fromTrieAC because we build the tree
  depth-first and strictly (due to IntMap.Strict). If we could build it breadth-first, then we
  could be strict in these, but I don't see an easy way to do that.

For complexities below, k is the alphabet range (max 256).

emptyTAC
An empty trie.

insertTAC
Inserts a string with an associated value into a trie. O(n log k) where n is the length of the
string.

fromListTAC
Builds a trie from a list of strings and associated values. O(n log k) where n is total length of
the strings.

fromTrieAC
Builds an Aho-Corasick automaton from a trie. O(n), where n is the number of nodes in the trie.
This is not more than the total length of strings the trie was constructed with.

matchAC
Returns a list of length (m + 1) where m is the length of the search string. This list contains a
list of pattern matches for every position in the string, including before the first character. A
match at a position is present as the associated value of the pattern string found to be ending at
that position.
O(m log k + z), where m is the length of the string and z is the total number of matches.
-}

module AhoCorasick
    ( TrieAC
    , emptyTAC
    , insertTAC
    , fromListTAC
    , ACRoot
    , fromTrieAC
    , matchAC
    ) where

import Control.Applicative
import Control.DeepSeq
import Data.List
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as IM

data ACRoot a = ACRoot !(IM.IntMap (ACNode a)) [a]
data ACNode a = ACNode !(IM.IntMap (ACNode a)) (Maybe (ACNode a)) [a]

fromTrieAC :: TrieAC a -> ACRoot a
fromTrieAC (TrieAC tm routs) = ACRoot rmp routs where
    rmp = IM.map go1 tm
    go1 (TrieAC m vs) = ACNode (IM.mapWithKey (go Nothing) m) Nothing (vs ++ routs)
    go psuf !c (TrieAC m vs) = ACNode (IM.mapWithKey (go suf) m) suf outs where
        suf = getSuf psuf
        getSuf Nothing                    = IM.lookup c rmp
        getSuf (Just (ACNode mp' suf' _)) = IM.lookup c mp' <|> getSuf suf'
        outs = vs ++ maybe routs (\(ACNode _ _ outs') -> outs') suf

matchAC :: ACRoot a -> B.ByteString -> [[a]]
matchAC (ACRoot rmp routs) !s0 = routs : gor s0 where
    gor s = case B.uncons s of
        Nothing -> []
        Just (c,s') -> case IM.lookup (fromEnum c) rmp of
            Nothing -> routs : gor s'
            Just (ACNode mp suf outs) -> outs : go mp suf s'
    go mp suf s = case B.uncons s of
        Nothing -> []
        Just (c, s') -> case IM.lookup (fromEnum c) mp of
            Nothing -> maybe gor (\(ACNode mp' suf' _) -> go mp' suf') suf s
            Just (ACNode mp' suf' outs) -> outs : go mp' suf' s'

data TrieAC a = TrieAC !(IM.IntMap (TrieAC a)) ![a] deriving Show

emptyTAC :: TrieAC a
emptyTAC = TrieAC IM.empty []

insertTAC :: B.ByteString -> a -> TrieAC a -> TrieAC a
insertTAC s v = go s where
    go cs (TrieAC m vs) = case B.uncons cs of
        Nothing       -> TrieAC m (v:vs)
        Just (c, cs') -> TrieAC m' vs where
            m' = IM.alter ((Just $!) . go cs' . fromMaybe emptyTAC) (fromIntegral c) m

fromListTAC :: [(B.ByteString, a)] -> TrieAC a
fromListTAC = foldl' (\t (s, v) -> insertTAC s v t) emptyTAC

--------------------------------------------------------------------------------
-- For tests

instance NFData a => NFData (ACNode a) where
    rnf (ACNode mp _outs suf) = suf `seq` rnf mp
-- outs of nodes share structure, so it is not forced
-- the suf link is forced only to WHNF, otherwise it would be reevaluating various parts of the tree

instance NFData a => NFData (ACRoot a) where
    rnf (ACRoot mp _outs) = rnf mp
