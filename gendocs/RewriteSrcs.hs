module Main where

import Control.Monad
import Data.Bifunctor
import Data.List
import System.Environment
import System.Exit
import System.FilePath

main :: IO ()
main = do
    names <- getArgs
    if null names
        then putStrLn "[RewriteSrcs] No input files"
        else do
            mapM_ processFile names
            putStrLn "[RewriteSrcs] Done"

processFile :: FilePath -> IO ()
processFile name | takeExtension name /= ".hs" = do
    putStrLn $ "[RewriteSrcs] Skipping (not .hs): " ++ name
processFile name = do
    putStrLn $ "[RewriteSrcs] Processing: " ++ name
    s <- readFileStrict name
    case processSrc s of
        Left er -> die $ "[RewriteSrcs] Error: " ++ name ++ ", " ++ er
        Right s' -> do
            writeFile name s'
            putStrLn $ "[RewriteSrcs] Done: " ++ name

---------------
-- Processing

-- A document looks like
--
-- {-|
-- Module info
--
-- Implementation notes:
-- * Note
-- * Note
--
-- func
-- Description of func.
-- -}
--
-- func :: Int -> Int
-- func x = x
--
-- Simple dumb processing based on lines:
-- 1. Split everything into lines
-- 2. Split out the module documentation between {-| and -}.
-- 3. Split the module doc and rest of the code into blocks by empty lines.
-- 4. Try to match each block from the module doc to a code block's signature.
-- 5. Move the block titled "Implementation notes:" to after the module doc.
-- 6. Make the heading of the module doc h2.
-- 7. Add a line before first bullets in the module doc (for Haddock to detect lists).
-- 8. Remove leftover empty lines in the module doc.
-- 9. Combine everything back.

type ErrStr = String
type Line = String

processSrc :: String -> Either ErrStr String
processSrc s = do
    (before, doc, rest) <- extractModuleDoc (lines s)
    let docs = splitOn (=="") doc
        xs = splitOn (=="") rest
        (docs', xs') = placeAllDocs docs xs
        (docs'', xs'') = case removeImplNotes docs' of
            Nothing -> (docs', xs')
            Just (implNotes, docs'') -> (docs'', [[""]] ++ [implNotes] ++ xs')
        docs''' = removeDup [""] . concatMap addListLine . setFirstH2 $ docs''
        combined = before ++ [open] ++ concat docs''' ++ [close] ++ concat xs''
    pure $ unlines combined

extractModuleDoc :: [Line] -> Either ErrStr ([Line], [Line], [Line])
extractModuleDoc ls = do
    (before, _, ls') <- note ("missing " ++ open) $ splitOn1 (==open) ls
    (doc, _, ls'') <- note ("missing " ++ close) $ splitOn1 (==close) ls'
    pure (before, doc, ls'')

placeAllDocs :: [[Line]] -> [[Line]] -> ([[Line]], [[Line]])
placeAllDocs [] xs = ([], xs)
placeAllDocs (doc:docs) xs = case placeDoc doc xs of
    Nothing  -> first (doc:) $ placeAllDocs docs xs
    Just xs' -> placeAllDocs docs xs'

placeDoc :: [Line] -> [[Line]] -> Maybe [[Line]]
placeDoc (name:doc) = replace1 f where
    f x = makeHaddockComment doc ++ x <$ guard (head x `defines` name)
placeDoc [] = error "placeDoc: doc has no body"

open, close :: String
open = "{-|"
close = "-}"

defines :: Line -> Line -> Bool
defines _   []   = False
defines sig name = or
    [ (name ++ " ::") `isPrefixOf` sig
    , ("data " ++ name) `isPrefixOf` sig
    , ("newtype " ++ name) `isPrefixOf` sig
    , ("class " ++ name) `isPrefixOf` sig
    , "class" `isPrefixOf` sig && ("=> " ++ name) `isInfixOf` sig
    ]

makeHaddockComment :: [Line] -> [Line]
makeHaddockComment = zipWith (++) ("-- | " : repeat "-- ")

addListLine :: [Line] -> [[Line]]
addListLine xs = case splitOn1 ("* " `isPrefixOf`) xs of
    Nothing -> [xs]
    Just (ys, x, zs) -> [ys] ++ [[""]] ++ [x:zs]

removeImplNotes :: [[Line]] -> Maybe ([Line], [[Line]])
removeImplNotes xss = f <$> splitOn1 (("Implementation notes:" `isPrefixOf`) . head) xss where
    f (yss, xs, zss) = (["{-"] ++ xs ++ ["-}"], yss ++ zss)

setFirstH2 :: [[Line]] -> [[Line]]
setFirstH2 ((x:xs):xss) = (("== " ++ x):xs):xss
setFirstH2 xss          = xss

----------
-- Utils

splitOn1 :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
splitOn1 p = go where
    go [] = Nothing
    go (x:xs)
        | p x       = Just ([], x, xs)
        | otherwise = (\(xs', y, zs) -> (x:xs', y, zs)) <$> go xs

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p = go where
    go xs = case break p xs of
        ([], [])   -> []
        ([], y:ys) -> [y] : go ys
        (zs, ys)   -> zs : go ys

replace1 :: (a -> Maybe a) -> [a] -> Maybe [a]
replace1 f = go where
    go [] = Nothing
    go (x:xs) = case f x of
        Nothing -> (x:) <$> go xs
        Just x' -> Just (x':xs)

removeDup :: Eq a => a -> [a] -> [a]
removeDup _ []       = []
removeDup x ys@(y:_) = y : [z' | (z, z') <- zip ys (tail ys), x /= z || x /= z']

-- readFile' exists but in >= GHC 9.0
readFileStrict :: FilePath -> IO String
readFileStrict name = do
    s <- readFile name
    length s `seq` pure s

note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right
