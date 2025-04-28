module TypeChecker (typecheck) where

import CQLParser (Stmt (..), VarName)
import Control.Monad.State (MonadIO (liftIO), StateT, modify)
import Data.Char (isDigit, toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Types (CSVData)
import Util (readCSV, warning)

type ColumnCount = Int

data CSVType = TString ColumnCount | TInt ColumnCount | TFloat ColumnCount | TBool ColumnCount deriving (Show, Eq)

type CSVTypeMap = Map VarName CSVType

type TypeChecker = StateT CSVTypeMap IO

typecheck :: Stmt -> TypeChecker ()
typecheck (Import filepath var) = do
  result <- liftIO $ readCSV filepath
  let csvType = typeOf var result
  liftIO $ print csvType
  modify $ Map.insert var csvType
typecheck (Set _ _) = undefined
typecheck (Map _ _ _) = undefined
typecheck _ = undefined

typeOf :: VarName -> CSVData -> CSVType
typeOf _ [] = TString 0
typeOf var rows@(row : _)
  | not (isEqualLengthColumns rows) = warning ("All rows must have the same number of columns in variable: " ++ "\"" ++ var ++ "\"") (TString (length row))
  | isIntType rows = TInt (length row)
  | isFloatType rows = TFloat (length row)
  | isBoolType rows = TBool (length row)
  | otherwise = TString (length row)

isIntType :: CSVData -> Bool
isIntType [] = True
isIntType csv = all (all isInt) csv
  where
    isInt :: String -> Bool
    isInt [] = False
    isInt (x : xs) = (x == '-' && not (null xs) && all isDigit xs) || (isDigit x && all isDigit xs)

isFloatType :: CSVData -> Bool
isFloatType [] = True
isFloatType csv = all (all isFloat) csv
  where
    isFloat :: String -> Bool
    isFloat [] = False
    isFloat s = case reads s :: [(Double, String)] of
      [(_, "")] -> True
      _ -> False

isBoolType :: CSVData -> Bool
isBoolType [] = True
isBoolType csv = all (all isBool) csv
  where
    isBool :: String -> Bool
    isBool s = map toLower s `elem` ["true", "false"]

isEqualLengthColumns :: CSVData -> Bool
isEqualLengthColumns [] = True
isEqualLengthColumns (firstRow : restRows) =
  all (\row -> length row == length firstRow) restRows