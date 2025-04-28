module TypeChecker (typecheck) where

import CQLParser (Expr (..), Query (..), Stmt (..), VarName)
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT, modify)
import Data.Char (isDigit, toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Types (CSVData)
import Util (readCSV, warning)

type ColumnCount = Int

data CSVType = TString ColumnCount | TInt ColumnCount | TFloat ColumnCount | TBool ColumnCount deriving (Show, Eq)

type CSVTypeMap = Map VarName CSVType

type TypeChecker = StateT CSVTypeMap IO

lookupType :: VarName -> TypeChecker CSVType
lookupType var = do
  ctx <- get
  maybe (warning ("Variable " ++ var ++ " not found") undefined) return (Map.lookup var ctx)

typecheck :: Stmt -> TypeChecker ()
typecheck (Import filepath var) = do
  result <- liftIO $ readCSV filepath
  let csvType = typeOf var result
  liftIO $ print csvType
  modify $ Map.insert var csvType
typecheck (Map expr varIn _) = do
  t <- lookupType varIn
  case t of
    TString _ -> case expr of
      ToUpper -> return ()
      ToLower -> return ()
      _ -> error ("Type mismatch - String CSV data \"" ++ varIn ++ "\" can only be mapped with UPPER or LOWER")
    TBool _ -> case expr of
      Not -> return ()
      _ -> error ("Type mismatch - Boolean CSV data \"" ++ varIn ++ "\" can only be mapped with NOT")
    _ -> case expr of
      AddN _ -> return ()
      SubN _ -> return ()
      _ -> error ("Type mismatch - Integer/Float CSV data \"" ++ varIn ++ "\" can only be mapped with addition or subtraction")
typecheck (Set var queries) = mapM_ (queryHandler var) queries
typecheck _ = return ()

queryHandler :: VarName -> Query -> TypeChecker ()
queryHandler _ query = do
  case query of
    Merge _ var1 var2 _ -> do
      t1 <- lookupType var1
      t2 <- lookupType var2
      case (getColumns t1, getColumns t2) of
        (c1, c2) -> if c1 == c2 then return () else warning "Type mismatch - Merge operation requires two variables with the same number of columns" return ()
    _ -> return ()

  return ()

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
    isInt (x : xs) = x == '-' && not (null xs) && all isDigit xs || isDigit x && all isDigit xs

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

getColumns :: CSVType -> Int
getColumns (TString c) = c
getColumns (TInt c) = c
getColumns (TFloat c) = c
getColumns (TBool c) = c
