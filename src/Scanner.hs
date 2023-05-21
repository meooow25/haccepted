{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, TypeApplications #-}
{-|
Fast input

Reads space separated values from ASCII input, the typical format in competitive programming.
This is overkill in most situations. For more information see
https://github.com/meooow25/haccepted/wiki/Performance-tips#input-and-output.

Implementation notes:
* Yes, it uses strict Bytestring and not lazy, because it's faster. This means the input stream is
  fully loaded into memory, but that's fine for typical input file sizes (< ~10 MB).

runS
Runs the given action with standard input.

charS
Reads a Char.

byteStrS
Reads a ByteString.

intS
Reads an Int.

ioArrS
Reads an array mutable in IO.

arrS
Reads an array.

uArrayS
Reads a UArray.

arrayS
Reads an Array.

graphS
Reads an undirected graph from an edge list.

graphDirS
Reads a directed graph from an edge list.
-}

module Scanner
    ( S
    , runS
    , charS
    , byteStrS
    , intS
    , ioArrS
    , arrS
    , uArrayS
    , arrayS
    , graphS
    , graphDirS
    ) where

import Control.Monad.State.Strict
import Data.Array
import Data.Array.Base
import Data.Array.IO
import Data.Graph
import Data.Maybe
import qualified Data.ByteString.Char8 as C

import Misc ( modifyArray' )

type S = StateT C.ByteString IO

runS :: S a -> IO a
runS s = C.getContents >>= evalStateT s

charS :: S Char
charS = state $ fromMaybe (error "charS: no char") . C.uncons . C.dropWhile (<=' ')

byteStrS :: S C.ByteString
byteStrS = state $ C.break (<=' ') . C.dropWhile (<=' ')

intS :: S Int
intS = state $ fromMaybe (error "intS: no int") . C.readInt . C.dropWhile (<=' ')

ioArrS :: (MArray a e IO, Ix i) => (i,i) -> S e -> S (a i e)
ioArrS bnds se = do
    a <- lift $ newArray_ bnds
    n <- lift $ getNumElements a
    forM_ [0..n-1] $ \i -> se >>= lift . unsafeWrite a i
    pure a
{-# INLINE ioArrS #-}

arrS :: forall a a' e i. (MArray a e IO, IArray a' e, Ix i) => (i,i) -> S e -> S (a' i e)
arrS bnds se = ioArrS @a bnds se >>= lift . unsafeFreeze
{-# INLINE arrS #-}

uArrayS :: (MArray IOUArray e IO, IArray UArray e, Ix i) => (i,i) -> S e -> S (UArray i e)
uArrayS = arrS @IOUArray

arrayS :: Ix i => (i,i) -> S e -> S (Array i e)
arrayS = arrS @IOArray

graphS :: Bounds -> Int -> S Graph
graphS bnds m = do
    g <- lift $ newArray bnds [] :: S (IOArray Vertex [Vertex])
    replicateM_ m $ do
        a <- intS
        b <- intS
        lift $ modifyArray' g a (b:) *> modifyArray' g b (a:)
    lift $ unsafeFreeze g

graphDirS :: Bounds -> Int -> S Graph
graphDirS bnds m = do
    g <- lift $ newArray bnds [] :: S (IOArray Vertex [Vertex])
    replicateM_ m $ do
        a <- intS
        b <- intS
        lift $ modifyArray' g a (b:)
    lift $ unsafeFreeze g
