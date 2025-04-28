module Util
  ( splitOn,
    statementPrinter,
    readCSV,
    convertToCSV,
    trimString,
    modifyAt,
    insertAfter,
    replaceWith,
    combineRows,
    writeToCSV,
    getColFromTable,
    getRowFromTable,
    setColInTable,
    setRowInTable,
  )
where

import CQLParser (Stmt (..))
import Data.List (intercalate)
import Types (CSVData, CSVRow)

splitOn :: Char -> String -> [String]
splitOn delimiter input =
  case break (== delimiter) input of
    (x, _ : xs) -> x : splitOn delimiter xs
    (x, _) -> [x]

statementPrinter :: Stmt -> IO ()
statementPrinter stmt = case stmt of
  Import file var -> do
    putStrLn $ "Import " ++ file ++ " as " ++ var
  Print var sort trim -> do
    putStrLn $ "Print " ++ var ++ " " ++ show sort ++ " " ++ show trim
  Write var file -> do
    putStrLn $ "Write " ++ var ++ " to " ++ file
  Set var query -> do
    putStrLn $ "Set " ++ var ++ " as " ++ show query
  Map var1 var2 var3 -> do
    putStrLn $ "Map " ++ show var1 ++ " " ++ var2 ++ " " ++ var3
  Access2D var1 colrow1 idx1 colrow2 idx2 -> do
    putStrLn $ "Access2D " ++ var1 ++ " " ++ show colrow1 ++ " " ++ show idx1 ++ " " ++ show colrow2 ++ " " ++ show idx2
  Transpose var1 var2 -> do
    putStrLn $ "Transpose " ++ var1 ++ " " ++ var2
  PrintColRow colrow1 sort trim -> do
    putStrLn $ "PrintColRow " ++ show colrow1 ++ " " ++ show sort ++ " " ++ show trim

readCSV :: String -> IO CSVData
readCSV filepath = do
  csvContent <- readFile filepath
  let rows = lines csvContent
  let csvDataList = map (splitOn ',') rows
  return csvDataList

convertToCSV :: CSVData -> String
convertToCSV rows = unlines $ map (intercalate ",") rows

writeToCSV :: CSVData -> FilePath -> IO ()
writeToCSV csv filepath = writeFile filepath (convertToCSV csv)

trimString :: String -> String
trimString = foldr (\c acc -> if c == ' ' then acc else c : acc) ""

modifyAt :: Int -> (String -> String) -> [String] -> [String]
modifyAt i f xs = [if idx == i then f x else x | (idx, x) <- zip [0 ..] xs]

insertAfter :: [a] -> Int -> [a] -> [a]
insertAfter row colNum literals = take colNum row ++ literals ++ drop colNum row

replaceWith :: (Eq a) => a -> a -> [a] -> [a]
replaceWith old new = map (\s -> if s == old then new else s)

combineRows :: CSVData -> CSVData -> [(CSVRow, CSVRow)]
combineRows table1 table2 = [(row1, row2) | row1 <- table1, row2 <- table2]

getColFromTable :: CSVData -> Int -> [String]
getColFromTable table colNum = map (!! (colNum - 1)) table

getRowFromTable :: CSVData -> Int -> CSVRow
getRowFromTable table rowNum = table !! (rowNum - 1)

setColInTable :: CSVData -> Int -> [String] -> CSVData
setColInTable table colNum = zipWith (\row val -> take (colNum - 1) row ++ [val] ++ drop colNum row) table

setRowInTable :: CSVData -> Int -> CSVRow -> CSVData
setRowInTable table rowNum newRow =
  take (rowNum - 1) table ++ [newRow] ++ drop rowNum table