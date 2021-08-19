import Control.DeepSeq
import Control.Exception
import Data.Char
import Data.Either
import Data.List
import System.Environment
import Text.CSV
import Text.Layout.Table
import Text.Printf
import qualified Data.Map as M

data BenchRow = BenchRow
    { titleBR    :: !String
    , nameBR     :: !String
    , sizeBR     :: !Int
    , meanTimeBR :: !Double

    , lineBR     :: !String
    , fullNameBR :: !String
    } deriving Show

type BItem = (String, [Int], [BSubitem]) -- title, sizes, subitems
type BSubitem = (String, [Maybe Double]) -- name, mean times

header :: String
header = "Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB"

main :: IO ()
main = do
    [inCsvFile, outCsvFile, outFile] <- getArgs
    putStrLn $ "Input CSV file:      " ++ inCsvFile
    putStrLn $ "Output CSV file:     " ++ outCsvFile
    putStrLn $ "Output summary file: " ++ outFile

    inCsvData <- readFile inCsvFile
    existingCsvData <- evaluate . force =<< readFile outCsvFile
    let inRows          = parse inCsvData
        existingRows    = parse existingCsvData
        (rows, newRows) = chooseRows inRows existingRows
        outCsv          = unlines $ header : map lineBR rows
        outSummary      = formatItems $ rowsToItems rows

    putStrLn $ if null newRows
        then "No new/changed benchmarks"
        else show (length newRows) ++ " new/changed benchmarks:"
    putStr $ unlines $ map fullNameBR newRows
    writeFile outCsvFile outCsv
    writeFile outFile outSummary
    putStrLn "Done!"

parse :: String -> [BenchRow]
parse fileContents = rows where
    contents = stripSpace fileContents
    lines' = stripSpace <$> lines contents
    csv = either (error . show) id $ parseCSV "" contents
    rows | head lines' /= header       = error "unexpected headers"
         | length lines' /= length csv = error "number of parsed lines differ"
         | otherwise                   = zipWith makeRow (tail lines') (tail csv)

    stripSpace = dropWhile isSpace . dropWhileEnd isSpace

    makeRow line (fullName:meanTimeStr:_) = BenchRow title name size meanTime line fullName where
        (title, rest) = break (== '/') fullName
        (rsize, rrest) = break (== '/') $ reverse rest
        name = tail $ init $ reverse rrest
        size = read $ reverse rsize
        meanTime = read meanTimeStr
    makeRow _ _ = error "!"

chooseRows :: [BenchRow] -> [BenchRow] -> ([BenchRow], [BenchRow])
chooseRows newItems existingItems = (map (either id id) xs, rights xs) where
    xs = map f newItems
    -- Keep the existing value if it exists and the new value doesn't differ too much
    f item = case find ((fullNameBR item ==) . fullNameBR) existingItems of
        Nothing -> Right item
        Just item' ->
            if keepOld (meanTimeBR item') (meanTimeBR item)
                then Left item'
                else Right item
    keepOld old new = abs (new - old) / old < tolerance
    tolerance = 10 / 100 -- 10%

rowsToItems :: [BenchRow] -> [BItem]
rowsToItems rows = items where
    -- Keep titles in insertion order, names in insertion order, records in size order
    titleToItem :: M.Map String ([String], M.Map String (M.Map Int Double))
    titleToItem = foldl' ins M.empty rows where
        ins titleToItem (BenchRow title name size meanTime _ _) = M.alter f title titleToItem where
            f Nothing = Just ([name], M.singleton name $ M.singleton size meanTime)
            f (Just (names, nameToSubitem)) = Just (name:names, M.alter g name nameToSubitem) where
                g Nothing = Just $ M.singleton size meanTime
                g (Just sizeToTime) = Just $ M.insert size meanTime sizeToTime
    items = map f $ nub $ map titleBR rows where
        f title = (title, sizes, subitems) where
            (names, nameToSubitem) = titleToItem M.! title
            sizes = sort $ nub $ concatMap M.keys $ M.elems nameToSubitem
            subitems = map g $ nub $ reverse names where
                g name = (name, maybeTimes) where
                    sizeToTime = nameToSubitem M.! name
                    maybeTimes = map (sizeToTime M.!?) sizes

formatItems :: [BItem] -> String
formatItems items = result where
    result = intercalate "\n" [
            "────────────"
          , " Benchmarks"
          , "────────────"
          , "This is a generated summary file."
          , "For details see the CSV file and the respective benchmark source files."
          , ""
          , intercalate "\n" $ map formatItem items
        ]
    formatItem (title, sizes, subitems) = unlines [title, table] where
        headers = "Name" : map show sizes
        rows = [name : map (maybe "" formatTime) maybeTimes | (name, maybeTimes) <- subitems]
        table =
            tableString (replicate (length headers) def)
            unicodeS
            (titlesH headers)
            (map rowG rows)

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
