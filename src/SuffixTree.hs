{-# LANGUAGE ScopedTypeVariables #-}
module SuffixTree
    ( SufTreeEdge(..)
    , SufTreeNode(..)
    , SuffixTree(..)
    , valSufT
    , buildSufT
    ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.List
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IM

data SufTreeEdge a = SufTreeEdge !Int !Int !(SufTreeNode a)
data SufTreeNode a = SufTreeNode !a !(IM.IntMap (SufTreeEdge a))
data SuffixTree a = SuffixTree !C.ByteString !(SufTreeNode a) !(a -> Int -> a)

valSufT :: SufTreeNode a -> a
valSufT (SufTreeNode a _) = a

buildSufT :: (Int -> a) -> (a -> Int -> a) -> (a -> a -> a) -> C.ByteString -> SufTreeNode a
buildSufT fromLeaf updEdge merge ss = mkNode (C.length ss) 0 where
    (nxt, left, len) = buildSufT_ (C.length ss) (fromEnum . C.index ss)
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
                    readArray nxt v >>= writeArray nxt v . IM.insert (at i) w
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
                        writeArray nxt w (IM.singleton wantc v)
                        writeArray left v (leftv + lenu')
                        writeArray len v (lenv - lenu')
                        writeArray nxt u (IM.insert (at pos) w nxtu)
                        pure w

    foldM_ (\(u, pos) i -> step i pos u) (root, 0) [0 .. n-1]

    (,,) <$> unsafeFreeze nxt <*> unsafeFreeze left <*> unsafeFreeze len

--------------------------------------------------------------------------------
-- For tests

instance NFData a => NFData (SufTreeEdge a) where
    rnf (SufTreeEdge _ _ n) = rnf n

instance NFData a => NFData (SufTreeNode a) where
    rnf (SufTreeNode a nxt) = rnf a `seq` rnf nxt
