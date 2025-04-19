module TypeChecker (inferCSVSchema) where

import Types (BaseType(..), ColumnType(..), Type(..), CSVData)
import Data.List (transpose)

inferBaseType :: String -> BaseType
inferBaseType s
  | s == "True" || s == "False" = TBool
  | all (`elem` "-0123456789") s = TInt
  | otherwise = TString


commonType :: [BaseType] -> BaseType
commonType types
  | all (== TBool) types = TBool
  | all (`elem` [TBool, TInt]) types = TInt
  | otherwise = TString


inferColumnType :: String -> [String] -> ColumnType
inferColumnType name values =
  let types = map inferBaseType values
   in ColumnType name (commonType types)


inferCSVSchema :: CSVData -> [ColumnType]
inferCSVSchema [] = []
inferCSVSchema (header:rows) =
  let columns = transpose rows
   in zipWith inferColumnType header columns