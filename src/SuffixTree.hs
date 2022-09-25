{-# LANGUAGE ScopedTypeVariables #-}
module SuffixTree
    ( SufTreeEdge(..)
    , SufTreeNode(..)
    , SuffixTree(..)
    , valSufT
    , buildSufT
    , matchSufT
    , drawSufTreeNode
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

data SufTreeEdge a = SufTreeEdge !Int !Int !(SufTreeNode a)
data SufTreeNode a = SufTreeNode !a !(IM.IntMap (SufTreeEdge a))
data SuffixTree a = SuffixTree !(Int -> IM.Key) !(a -> Int -> a) !(SufTreeNode a)

valSufT :: SufTreeNode a -> a
valSufT (SufTreeNode a _) = a

buildSufT :: (Int -> a) -> (a -> Int -> a) -> (a -> a -> a) -> Int -> (Int -> IM.Key) -> SuffixTree a
buildSufT fromLeaf updEdge merge n at = SuffixTree at updEdge (mkNode n 0) where
    (nxt, left, len) = buildSufT_ n at
    mkNode dep i = SufTreeNode a nxt' where
        nxt' = (\j -> SufTreeEdge (left!j) (len!j) (mkNode (dep - len!j) j)) <$> nxt!i
        a | IM.null nxt' = fromLeaf dep
          | otherwise    = foldl1' merge [updEdge (valSufT v) len | SufTreeEdge _ len v <- IM.elems nxt']

buildSufT_ :: Int -> (Int -> IM.Key) -> (Array Int (IM.IntMap Int), UArray Int Int, UArray Int Int)
buildSufT_ n at = runST $ do
    let nn = 2 * n + 1
    nxt :: STArray s Int (IM.IntMap Int) <- newArray (0, nn) IM.empty
    suf :: STUArray s Int Int <- newArray (0, nn) 0
    left :: STUArray s Int Int <- newArray (0, nn) 0
    len :: STUArray s Int Int <- newArray (0, nn) 0

    cur :: STUArray s () Int <- newArray ((), ()) 1
    let root = 0
        nxtId = do
            i <- readArray cur ()
            writeArray cur () $ i + 1
            pure i
        step i = go root
          where
            go prv pos u = do
                nxtu <- readArray nxt u
                case nxtu IM.!? at pos of
                    Nothing -> insLeafGo u -- pos == i
                    Just v  -> join $ tryEdge nxtu v <$> readArray left v <*> readArray len v
              where
                setprvsuf v = writeArray suf prv v :: ST s ()
                goSuf v
                    | u /= root = readArray suf u >>= go v pos
                    | pos < i   = go v (pos + 1) root
                    | otherwise = pure (root, pos)
                insLeaf v = do
                    w <- nxtId
                    writeArray left w i :: ST s ()
                    writeArray len w (n - i)
                    nxtv <- readArray nxt v
                    writeArray nxt v $! IM.insert (at i) w nxtv
                insLeafGo v = setprvsuf v *> insLeaf v *> goSuf v
                tryEdge nxtu v leftv lenv
                    | pos + lenv <= i = go prv (pos + lenv) v
                    | at i == wantc = if pos == leftv then goSuf u else (u, pos) <$ setprvsuf u
                    | otherwise = doSplit >>= insLeafGo
                  where
                    lenu' = i - pos
                    wantc = at (leftv + lenu')
                    doSplit = do
                        w <- nxtId
                        writeArray left w leftv
                        writeArray len w lenu'
                        writeArray nxt w $! IM.singleton wantc v
                        writeArray left v (leftv + lenu')
                        writeArray len v (lenv - lenu')
                        writeArray nxt u $! IM.insert (at pos) w nxtu
                        pure w

    foldM_ (\(u, pos) i -> step i pos u) (root, 0) [0 .. n-1]

    (,,) <$> unsafeFreeze nxt <*> unsafeFreeze left <*> unsafeFreeze len

matchSufT :: SuffixTree a -> Int -> (Int -> IM.Key) -> Maybe a
matchSufT (SuffixTree at updEdge u) m at' = go u 0 where
    go (SufTreeNode a _) i | i == m = Just a
    go (SufTreeNode _ nxt) i = IM.lookup (at' i) nxt >>= go' where
        go' (SufTreeEdge left len v)
            | d == len  = go v i'
            | i' == m   = Just (updEdge (valSufT v) (len - d))
            | otherwise = Nothing
          where
            d = commonPrefix (min len (m - i)) (at . (+left)) (at' . (+i))
            i' = i + d
    commonPrefix n f g = go 0 where
        go i = if i < n && f i == g i then go (i + 1) else i

--------------------------------------------------------------------------------
-- For tests

instance NFData a => NFData (SufTreeEdge a) where
    rnf (SufTreeEdge _ _ n) = rnf n

instance NFData a => NFData (SufTreeNode a) where
    rnf (SufTreeNode a nxt) = rnf a `seq` rnf nxt

instance NFData a => NFData (SuffixTree a) where
    rnf (SuffixTree _ _ u) = rnf u

drawSufTreeNode :: Show a => SufTreeNode a -> String
drawSufTreeNode = draw (show . valSufT) nbs where
    nbs (SufTreeNode _ nxt) = [(Just (show (left, len)), v)| SufTreeEdge left len v <- IM.elems nxt]
