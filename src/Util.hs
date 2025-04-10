module Util (splitOn, statementPrinter, readCSV, convertToCSV, trimString) where

import CQLParser (Stmt (..))
import Data.List (intercalate)
import Types (CSVData)

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

readCSV :: String -> IO CSVData
readCSV filepath = do
  csvContent <- readFile filepath
  let rows = lines csvContent
  let csvDataList = map (splitOn ',') rows
  return csvDataList

convertToCSV :: CSVData -> String
convertToCSV rows = unlines $ map (intercalate ",") rows

trimString :: String -> String
trimString = foldr (\c acc -> if c == ' ' then acc else c : acc) ""