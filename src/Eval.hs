module Eval (eval) where

import CQLParser (ColRowData (..), FilterQuery (..), MergeType (..), Operand (..), Operator (..), Query (..), SortOrder (..), Stmt (..), Trim (..), VarName)
import Control.Monad.State
import Data.List (nub, sort, sortBy, transpose)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import State (addToContext)
import Types (CSVData, CSVRow, CSVState)
import Util (combineRows, convertToCSV, insertAfter, readCSV, replaceWith, trimString)

eval :: Stmt -> CSVState ()
eval (Import file var) = do
  result <- liftIO $ readCSV file
  addToContext var result
eval (Print var sortOrder trim) = do
  ctx <- get
  let result = Map.lookup var ctx
  liftIO $ putStr $ formatResult result sortOrder trim
eval (Set var queries) = do
  mapM_ (`queryHandler` var) queries
eval _ = return ()

formatResult :: Maybe CSVData -> SortOrder -> Trim -> String
formatResult Nothing _ _ = "Variable not found"
formatResult (Just result) sortOrder trim =
  let sortedData = sortData result sortOrder
      trimmedData = trimData sortedData trim
   in convertToCSV trimmedData

sortData :: CSVData -> SortOrder -> CSVData
sortData result ASC = sort result
sortData result DESC = sortBy (comparing Down) result

trimData :: CSVData -> Trim -> CSVData
trimData result TrimTrue = map (map trimString) result
trimData result TrimFalse = result

filterResult :: FilterQuery -> CSVState CSVData
filterResult (FilterColRowIsNotNull (ColRowData table _ colNum)) = do
  ctx <- get
  let tableData = fromJust (Map.lookup table ctx)
  let filteredData = filter (\row -> row !! (colNum - 1) /= "") tableData
  return filteredData
filterResult (FilterColRowIsNull (ColRowData table _ colNum)) = do
  ctx <- get
  let tableData = fromJust (Map.lookup table ctx)
  let filteredData = filter (\row -> row !! (colNum - 1) == "") tableData
  return filteredData
filterResult (FilterColRow (ColRowData table _ colNum) operator (ColRowData _ _ colNum2)) = do
  ctx <- get
  let tableData = fromJust (Map.lookup table ctx)

  let filteredData = filter (\row -> filterFunction operator (row !! (colNum - 1)) (row !! (colNum2 - 1))) tableData
  return filteredData
filterResult (FilterColRowOperand (ColRowData table _ colNum) operator operand) = do
  ctx <- get
  let tableData = fromJust (Map.lookup table ctx)
  let filteredData = case operand of
        OperandLiteral literal -> filter (\row -> filterFunction operator (row !! (colNum - 1)) literal) tableData
        OperandNum num -> filter (\row -> filterFunction operator (read (row !! (colNum - 1)) :: Double) (fromIntegral num)) tableData
        OperandFloat float -> filter (\row -> filterFunction operator (read (row !! (colNum - 1)) :: Double) (fromRational (toRational float))) tableData
  return filteredData

mergeTables :: MergeType -> CSVData -> CSVData -> Int -> CSVData
mergeTables mergeType table1 table2 colNum =
  let rowPairs = combineRows table1 table2
      filteredRowPairs = filter (\(row1, row2) -> row1 !! (colNum - 1) == row2 !! (colNum - 1)) rowPairs
      zippedRowPairs = map (uncurry zip) filteredRowPairs
   in mergeRows mergeType zippedRowPairs

mergeRows :: MergeType -> [[(String, String)]] -> [CSVRow]
mergeRows LeftMerge rowPairs = map (map (\(p, q) -> if null p then q else p)) rowPairs
mergeRows RightMerge rowPairs = map (map (\(p, q) -> if null q then p else q)) rowPairs

filterFunction :: (Ord a) => Operator -> a -> a -> Bool
filterFunction op operand1 operand2 = case op of
  Equal -> operand1 == operand2
  NotEqual -> operand1 /= operand2
  LessThan -> operand1 < operand2
  GreaterThan -> operand1 > operand2
  LessThanOrEqual -> operand1 <= operand2
  GreaterThanOrEqual -> operand1 >= operand2

queryHandler :: Query -> VarName -> CSVState ()
queryHandler query var = do
  ctx <- get
  case query of
    Distinct varName -> do
      let table = fromJust (Map.lookup varName ctx)
      let distinctData = nub table
      addToContext var distinctData
    Union vars -> do
      let tables = map (fromJust . (`Map.lookup` ctx)) vars
      let unionedData = nub $ concat tables
      addToContext var unionedData
    Merge mergeType var1 var2 colNum -> do
      let table1 = fromJust (Map.lookup var1 ctx)
      let table2 = fromJust (Map.lookup var2 ctx)
      let mergedData = mergeTables mergeType table1 table2 colNum
      addToContext var mergedData
    Filter filterQuery -> do
      filteredResult <- filterResult filterQuery
      addToContext var filteredResult
    Cross vars -> do
      let results = map (fromJust . (`Map.lookup` ctx)) vars
      let result = foldl (\acc row -> [row1 ++ row2 | row1 <- acc, row2 <- row]) [[]] results
      addToContext var result
    Get colRows -> do
      let columns = transpose $ map (\(ColRowData table _ colNum) -> let tableData = fromJust (Map.lookup table ctx) in map (!! (colNum - 1)) tableData) colRows
      addToContext var columns
    Concat (ColRowData table _ colNum) literals -> do
      let tableData = fromJust (Map.lookup table ctx)

      let updatedData = map (\row -> insertAfter row colNum literals) tableData
      let replacedData = map (\row -> let colValue = row !! (colNum - 1) in replaceWith "$" colValue row) updatedData

      addToContext var replacedData