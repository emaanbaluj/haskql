module Util (splitOn) where

splitOn :: Char -> String -> [String]
splitOn delimitter input =
  case break (== delimitter) input of
    (x, _ : xs) -> x : splitOn delimitter xs
    (x, _) -> [x]