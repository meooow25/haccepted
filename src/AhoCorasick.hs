{-
Aho-Corasick algorithm

The Aho-Corasick algorithm builds an automaton from a set of pattern strings, and then uses it to
find positions in a search string where each of the pattern strings occur.

This implementation only works on ByteStrings, to keep things fast. If required it can be adapted
to work on Strings, or even more generally (Ord a, Foldable f) => f a.

A TrieAC a can be constructed from pattern strings with associated values a, which can be then be
turned into an ACRoot a. An ACRoot a can then be run on a search string to find matches.
Construction and matching are both lazy.

Sources:
* Alfred V. Aho and Margaret J. Corasick, "Efficient string matching: An aid to bibliographic
  search", 1975
  https://dl.acm.org/doi/10.1145/360825.360855
* Stanford CS166 Aho-Corasick lecture slides
  https://web.stanford.edu/class/archive/cs/cs166/cs166.1166/lectures/04/Slides04.pdf

Let k be the alphabet size. Let the complexity of IntMap operations be f(n), where n is the size of
the map. f(n) is O(min(n, word size)), see IntMap documentation for details.

emptyTAC
An empty trie.

insertTAC
Inserts a string with an associated value into a trie. O(n * f(k)) where n is the length of the
string.

fromListTAC
Builds a trie from a list of strings and associated values. O(n * f(k)) where n is total length of
the strings.

fromTrieAC
Builds an Aho-Corasick automaton from a trie. O(n), where n is the number of nodes in the trie.
This is not more than the total length of strings the trie was constructed with.

matchAC
Returns a list of length (m + 1) where m is the length of the search string. This list contains a
list of pattern matches for every position in the string, including before the first character. A
match at a position is present as the associated value of the pattern string found to be ending at
that position.
O(m * f(k) + z), where m is the length of the string and z is the total number of matches.
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

import Control.DeepSeq
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IM

data ACRoot a = ACRoot (IM.IntMap (ACNode a)) [a]
data ACNode a = ACNode (IM.IntMap (ACNode a)) [a] (ACLink a)
data ACLink a = RootL | NodeL !(ACNode a)

fromTrieAC :: TrieAC a -> ACRoot a
fromTrieAC (TrieAC tm tvs) = ACRoot rmp tvs where
    rmp = IM.map go1 tm
    go1 (TrieAC m vs) = ACNode (IM.mapWithKey (go RootL) m) (vs ++ tvs) RootL
    go psuf c (TrieAC m vs) = ACNode (IM.mapWithKey (go suf) m) outs suf where
        suf = getSuf psuf
        getSuf RootL                       = maybe RootL NodeL (IM.lookup c rmp)
        getSuf (NodeL (ACNode mp' _ suf')) = maybe (getSuf suf') NodeL (IM.lookup c mp')
        outs = vs ++ case suf of
            RootL                    -> tvs
            NodeL (ACNode _ outs' _) -> outs'

matchAC :: ACRoot a -> C.ByteString -> [[a]]
matchAC (ACRoot rmp routs) = (routs:) . go1 where
    go1 s = case C.uncons s of
        Nothing      -> []
        Just (c, s') -> case IM.lookup (fromEnum c) rmp of
            Nothing                  -> routs : go1 s'
            Just x@(ACNode _ outs _) -> outs : go s' x
    go s (ACNode mp _ suf) = case C.uncons s of
        Nothing      -> []
        Just (c, s') -> case IM.lookup (fromEnum c) mp of
            Nothing -> case suf of
                RootL   -> go1 s
                NodeL x -> go s x
            Just x@(ACNode _ outs _) -> outs : go s' x

data TrieAC a = TrieAC (IM.IntMap (TrieAC a)) [a] deriving Show

emptyTAC :: TrieAC a
emptyTAC = TrieAC IM.empty []

insertTAC :: C.ByteString -> a -> TrieAC a -> TrieAC a
insertTAC s v = go s where
    go cs (TrieAC m vs) = case C.uncons cs of
        Nothing       -> TrieAC m (v:vs)
        Just (c, cs') -> TrieAC m' vs where
            m' = IM.alter (Just . go cs' . fromMaybe emptyTAC) (fromEnum c) m

fromListTAC :: [(C.ByteString, a)] -> TrieAC a
fromListTAC = foldl' (\t (s, v) -> insertTAC s v t) emptyTAC

--------------------------------------------------------------------------------
-- For tests

-- outs of nodes share structure, so rnf is O(n^2)
instance NFData a => NFData (ACNode a) where
    rnf (ACNode mp outs suf) = rnf outs `seq` suf `seq` rnf mp

instance NFData a => NFData (ACRoot a) where
    rnf (ACRoot mp outs) = rnf outs `seq` rnf mp
