module Util (splitOn, statementPrinter) where

import CQLParser (Stmt (..))

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