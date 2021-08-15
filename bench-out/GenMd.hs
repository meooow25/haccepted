{-# LANGUAGE OverloadedStrings #-}

import Data.Csv
import Data.List
import Data.Text.Read
import System.Environment
import Text.Printf
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

data BenchRow = BenchRow !T.Text !T.Text !Int !Double -- title, name, size, mean time

instance FromNamedRecord BenchRow where
    parseNamedRecord r = f <$> r .: "Name" <*> r .: "Mean" where
        f fullName meanTime = BenchRow title name size meanTime where
            (title, rest) = T.breakOn "/" fullName
            (rest', sizeStr) = T.breakOnEnd "/" rest
            name = T.tail $ T.init rest'
            size = case decimal sizeStr of
                Left err -> error err
                Right (x, _) -> x

type BItem = (T.Text, [Int], [BSubitem]) -- title, sizes, subitems
type BSubitem = (T.Text, [Maybe Double]) -- name, mean times

main :: IO ()
main = do
    [inCsvFile, outMdFile] <- getArgs
    putStrLn $ "Input CSV file:       " ++ inCsvFile
    putStrLn $ "Output Markdown file: " ++ outMdFile
    csvData <- BL.readFile inCsvFile
    let result = case decodeByName csvData of
            Left err -> error err
            Right (_, rows) -> formatItems $ rowsToItems rows
    TIO.writeFile outMdFile result
    putStrLn "Done!"

rowsToItems :: V.Vector BenchRow -> [BItem]
rowsToItems rows = items where
    -- Keep titles in insertion order, names in insertion order, records in size order
    titleToItem :: M.Map T.Text ([T.Text], M.Map T.Text (M.Map Int Double))
    titleToItem = V.foldl' ins M.empty rows where
        ins titleToItem (BenchRow title name size meanTime) = M.alter f title titleToItem where
            f Nothing = Just ([name], M.singleton name $ M.singleton size meanTime)
            f (Just (names, nameToSubitem)) = Just (name:names, M.alter g name nameToSubitem) where
                g Nothing = Just $ M.singleton size meanTime
                g (Just sizeToTime) = Just $ M.insert size meanTime sizeToTime
    items = map f $ nub $ [title | BenchRow title _ _ _ <- V.toList rows] where
        f title = (title, sizes, subitems) where
            (names, nameToSubitem) = titleToItem M.! title
            sizes = sort $ nub $ concatMap M.keys $ M.elems nameToSubitem
            subitems = map g $ nub $ reverse names
            g name = (name, maybeTimes) where
                sizeToTime = nameToSubitem M.! name
                maybeTimes = map (sizeToTime M.!?) sizes

formatItems :: [BItem] -> T.Text
formatItems items = result where
    result = T.intercalate "\n" [
            "# Benchmarks"
          , ""
          , T.intercalate "\n" $ map formatItem items
        ]
    formatItem (title, sizes, subitems) = result where
        headers = "Name" : map (T.pack . show) sizes
        result = T.intercalate "\n" [
                "### " <> title
              , ""
              , formatRow headers
              , formatRow $ map (const "---") headers
              , T.intercalate "\n" $ map formatSubitem subitems
              , ""
            ]
    formatSubitem (name, maybeTimes) =
        formatRow $ name : map (maybe "" (T.pack . formatTime)) maybeTimes
    formatRow xs = "| " <> T.intercalate " | " xs <> " |"

-- https://github.com/haskell/criterion/blob/3f6a32b01dbadb5c0954aaebaf4fef923e7ac7ec/criterion-measurement/src/Criterion/Measurement.hs#L372-L391
formatTime :: Double -> String
formatTime k
    | k < 0      = '-' : formatTime (-k)
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "μs"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | k >= 1e-15 = (k*1e15) `with` "fs"
    | k >= 1e-18 = (k*1e18) `with` "as"
    | otherwise  = printf "%g s" k
    where 
        with :: Double -> String -> String
        with t u
            | t >= 1e9  = printf "%.4g %s" t u
            | t >= 1e3  = printf "%.0f %s" t u
            | t >= 1e2  = printf "%.1f %s" t u
            | t >= 1e1  = printf "%.2f %s" t u
            | otherwise = printf "%.3f %s" t u
