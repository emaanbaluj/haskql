module TypeChecker (typecheck) where

import CQLParser (ColRow (..), Expr (..), Query (..), Stmt (..), VarName)
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT, modify)
import Data.Char (isDigit, toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Types (CSVData)
import Util (allSame, readCSV, warning)
import Control.DeepSeq (NFData (rnf))

type ColumnCount = Int

type RowCount = Int

data CSVType = TString ColumnCount RowCount | TInt ColumnCount RowCount | TFloat ColumnCount RowCount | TBool ColumnCount RowCount deriving (Show, Eq)

instance NFData CSVType where
  rnf (TString c r) = c `seq` r `seq` ()
  rnf (TInt c r) = c `seq` r `seq` ()
  rnf (TFloat c r) = c `seq` r `seq` ()
  rnf (TBool c r) = c `seq` r `seq` ()

type CSVTypeMap = Map VarName CSVType

type TypeChecker = StateT CSVTypeMap IO

lookupType :: VarName -> TypeChecker CSVType
lookupType var = do
  ctx <- get
  maybe (warning ("Variable " ++ var ++ " not found") return (TString 0 0)) return (Map.lookup var ctx)

typecheck :: Stmt -> TypeChecker ()
typecheck (Insert ROW literals varIn varOut) = do
  t <- lookupType varIn
  case t of
    TString c r -> do
      if length literals == c then return () else warning ("Type mismatch - Insert ROW operation requires " ++ show c ++ " literals") return ()
      modify $ Map.insert varOut (TString c (r + 1))
    TInt c r -> do
      if length literals == c then return () else warning ("Type mismatch - Insert ROW operation requires " ++ show c ++ " literals") return ()
      modify $ Map.insert varOut (TInt c (r + 1))
    TFloat c r -> do
      if length literals == c then return () else warning ("Type mismatch - Insert ROW operation requires " ++ show c ++ " literals") return ()
      modify $ Map.insert varOut (TFloat c (r + 1))
    TBool c r -> do
      if length literals == c then return () else warning ("Type mismatch - Insert ROW operation requires " ++ show c ++ " literals") return ()
      modify $ Map.insert varOut (TBool c (r + 1))
typecheck (Insert COL literals varIn varOut) = do
  t <- lookupType varIn
  case t of
    TString c r -> do
      if length literals == r then return () else warning ("Type mismatch - Insert COL operation requires " ++ show r ++ " literals") return ()
      modify $ Map.insert varOut (TString (c + 1) r)
    TInt c r -> do
      if length literals == r then return () else warning ("Type mismatch - Insert COL operation requires " ++ show r ++ " literals") return ()
      modify $ Map.insert varOut (TInt (c + 1) r)
    TFloat c r -> do
      if length literals == r then return () else warning ("Type mismatch - Insert COL operation requires " ++ show r ++ " literals") return ()
      modify $ Map.insert varOut (TFloat (c + 1) r)
    TBool c r -> do
      if length literals == r then return () else warning ("Type mismatch - Insert COL operation requires " ++ show r ++ " literals") return ()
      modify $ Map.insert varOut (TBool (c + 1) r)
typecheck (Transpose varIn varOut) = do
  t <- lookupType varIn
  case t of
    TString c r -> modify $ Map.insert varOut (TString r c)
    TInt c r -> modify $ Map.insert varOut (TInt r c)
    TFloat c r -> modify $ Map.insert varOut (TFloat r c)
    TBool c r -> modify $ Map.insert varOut (TBool r c)
  return ()
typecheck (Import filepath var) = do
  result <- liftIO $ readCSV filepath
  let csvType = typeOf var result
  modify $ Map.insert var csvType
typecheck (Map expr varIn _) = do
  t <- lookupType varIn
  case t of
    TString _ _ -> case expr of
      ToUpper -> return ()
      ToLower -> return ()
      _ -> warning ("Type mismatch - String CSV data \"" ++ varIn ++ "\" can only be mapped with UPPER or LOWER") return ()
    TBool _ _ -> case expr of
      Not -> return ()
      _ -> warning ("Type mismatch - Boolean CSV data \"" ++ varIn ++ "\" can only be mapped with NOT") return ()
    _ -> case expr of
      AddN _ -> return ()
      SubN _ -> return ()
      _ -> warning ("Type mismatch - Integer/Float CSV data \"" ++ varIn ++ "\" can only be mapped with addition or subtraction") return ()
typecheck (Set var queries) = mapM_ (queryHandler var) queries
typecheck _ = return ()

queryHandler :: VarName -> Query -> TypeChecker ()
queryHandler _ query = do
  case query of
    Stack vars -> do
      ts <- mapM lookupType vars
      let columns = map getColumnCounts ts
      if allSame columns then return () else warning ("Type mismatch - Stack operation requires all variables " ++ show vars ++ " to have the same number of columns") return ()
      if allSameDataType ts then return () else warning ("Type mismatch - Stack operation requires all variables " ++ show vars ++ " to have the same type for type safety") return ()
      return ()
    Zip vars -> do
      ts <- mapM lookupType vars
      let rows = map getRowCounts ts
      if allSame rows then return () else warning ("Type mismatch - Zip operation requires all variables " ++ show vars ++ " to have the same number of rows") return ()
      if allSameDataType ts then return () else warning ("Type mismatch - Zip operation requires all variables " ++ show vars ++ " to have the same type for type safety") return ()
      return ()
    Cross vars -> do
      ts <- mapM lookupType vars
      let columns = map getColumnCounts ts
      if allSameDataType ts then return () else warning ("Type mismatch - Cross operation requires all variables " ++ show vars ++ " to have the same type for type safety") return ()
      return ()
    Merge _ var1 var2 _ -> do
      t1 <- lookupType var1
      t2 <- lookupType var2
      case (getColumnCounts t1, getColumnCounts t2) of
        (c1, c2) -> if c1 == c2 then return () else warning "Type mismatch - Merge operation requires two variables with the same number of columns" return ()
    _ -> return ()

  return ()

typeOf :: VarName -> CSVData -> CSVType
typeOf _ [] = TString 0 0
typeOf var rows@(row : _)
  | not (isEqualLengthRows rows) = warning ("All rows must have the same length in table: " ++ "\"" ++ var ++ "\"") (TString (length row) (length rows))
  | isIntType rows = TInt (length row) (length rows)
  | isFloatType rows = TFloat (length row) (length rows)
  | isBoolType rows = TBool (length row) (length rows)
  | otherwise = TString (length row) (length rows)

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

isEqualLengthRows :: CSVData -> Bool
isEqualLengthRows [] = True
isEqualLengthRows (firstRow : restRows) =
  all (\row -> length row == length firstRow) restRows

getColumnCounts :: CSVType -> Int
getColumnCounts (TString c _) = c
getColumnCounts (TInt c _) = c
getColumnCounts (TFloat c _) = c
getColumnCounts (TBool c _) = c

getRowCounts :: CSVType -> Int
getRowCounts (TString _ r) = r
getRowCounts (TInt _ r) = r
getRowCounts (TFloat _ r) = r
getRowCounts (TBool _ r) = r

allSameDataType :: [CSVType] -> Bool
allSameDataType [] = True
allSameDataType (x : xs) = all (sameTypeAs x) xs
  where
    sameTypeAs (TString _ _) (TString _ _) = True
    sameTypeAs (TInt _ _) (TInt _ _) = True
    sameTypeAs (TFloat _ _) (TFloat _ _) = True
    sameTypeAs (TBool _ _) (TBool _ _) = True
    sameTypeAs _ _ = False