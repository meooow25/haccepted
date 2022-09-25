{-# LANGUAGE ScopedTypeVariables #-}
module SuffixTree
    ( SufTNode(..)
    , SufTEdge(..)
    , buildSufT
    , matchSufT
    , buildMatchSufT
    , drawSufTNode
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

drawSufTNode :: Show a => SufTNode a -> String
drawSufTNode = draw showa nbs where
    showa (SufTNode a _) = show a
    nbs (SufTNode _ nxt) = [(Just (show (left, len)), v)| SufTEdge left len v <- IM.elems nxt]
