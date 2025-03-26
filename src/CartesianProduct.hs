module CartesianProduct where

import Data.List (intercalate)

processCP :: FilePath -> FilePath -> IO ()
processCP csv1 csv2 = do
  contentA <- readFile csv1
  contentB <- readFile csv2

  let lines1 = lines contentA
      lines2 = lines contentB
      rows1 = map (splitOn ',') lines1
      rows2 = map (splitOn ',') lines2
      cartesianProduct = [a ++ b | a <- rows1, b <- rows2]
      outputLines = map (intercalate ",") cartesianProduct

  writeFile "output.csv" (unlines outputLines)
  putStrLn "Cartesian Product written to output.csv"

splitOn :: Char -> String -> [String]
splitOn delimitter input =
  case break (== delimitter) input of
    (x, _ : xs) -> x : splitOn delimitter xs
    (x, _) -> [x]