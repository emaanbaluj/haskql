module Eval (eval) where
import Debug.Trace (traceM)
import CQLParser (ColRowData(..), FilterQuery(..), MergeType(..), Operand(..), Operator(..), Query(..), SortOrder(..), Stmt(..), Trim(..), VarName, Expr(..), ColRow(..))
import Control.Monad.State
import Data.List (nub, sort, sortBy, transpose)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import State (addToContext)
import Types (CSVData, CSVRow, CSVState)
import Data.Matrix (Matrix, fromLists, toLists, (<->))
import Util (combineRows, convertToCSV, insertAfter, readCSV, replaceWith, trimString, writeToCSV)
import Data.Char (toUpper, toLower)

-- Main evaluation entry point
eval :: Stmt -> CSVState ()

eval (Access2D var firstAxis firstIdx secondAxis secondIdx) = do
  ctx <- get
  case Map.lookup var ctx of
    Nothing -> liftIO $ putStrLn ("<error> no such table: " ++ var)
    Just table -> do 
      let value = access2D table firstAxis firstIdx secondAxis secondIdx
      liftIO $ putStrLn value

eval (PrintColRow (ColRowData var COL colNum) sort trim) = do
  ctx <- get
  case Map.lookup var ctx of
    Nothing -> liftIO $ putStrLn ("<error> no such table: " ++ var)
    Just table -> do
      let column = map (!! (colNum - 1)) table
          trimmed = case trim of
                      TrimTrue -> map trimString column
                      TrimFalse -> column
      liftIO $ putStr (unlines trimmed)


eval (PrintColRow (ColRowData var ROW rowNum) sort trim) = do
  ctx <- get
  case Map.lookup var ctx of
    Nothing -> liftIO $ putStrLn ("<error> no such table: " ++ var)
    Just table -> do
      let row =  table !! (rowNum -1)
          trimmed = case trim of
                      TrimTrue -> map trimString row
                      TrimFalse -> row
      liftIO $ putStr (unwords trimmed)


eval (Transpose input output) = do
  ctx <- get
  case Map.lookup input ctx of
    Nothing -> liftIO $ putStrLn ("<error> no such table: " ++ input)
    Just table -> addToContext output (transpose table)

-- MAP (expr) IN input AS output
eval (Map expr input output) = do
  ctx <- get
  case Map.lookup input ctx of
    Nothing -> liftIO $ putStrLn ("<error> no such table: " ++ input)
    Just (header:rows) -> do
      let newRows = map (map (apply expr)) rows
      addToContext output (header : newRows)

-- WRITE var TO file
eval (Write var filePath) = do
  ctx <- get
  case Map.lookup var ctx of
    Nothing    -> liftIO $ putStrLn ("<error> no such variable: " ++ var)
    Just table -> liftIO $ writeToCSV table filePath

-- IMPORT file AS var
eval (Import file var) = do
  result <- liftIO $ readCSV file
  addToContext var result

-- PRINT var
eval (Print var sortOrder trim) = do
  ctx <- get
  let result = Map.lookup var ctx
  liftIO $ putStr $ formatResult result
  where
    formatResult :: Maybe CSVData -> String
    formatResult Nothing = "Variable not found"
    formatResult (Just result) =
      let sortedData = sortData result sortOrder
          trimmedData = trimData sortedData trim
       in convertToCSV trimmedData

    sortData :: CSVData -> SortOrder -> CSVData
    sortData result NO   = result
    sortData result ASC = sort result
    sortData result DESC = sortBy (comparing Down) result

    trimData :: CSVData -> Trim -> CSVData
    trimData result TrimTrue = map (map trimString) result
    trimData result TrimFalse = result

-- SET var AS queries
eval (Set var queries) = do
  mapM_ (queryHandler var) queries

-- Fallback
eval _ = return ()

--------------------------------------------------------------------------------
-- Helper functions

-- Apply transformation to each cell
apply :: Expr -> String -> String
apply (AddN n) s = case reads s of
  [(x, "")] -> show (x + n :: Int)
  _         -> s

apply (SubN n) s = case reads s of
  [(x, "")] -> show (x - n :: Int)
  _         -> s

apply ToUpper s = map toUpper s
apply ToLower s = map toLower s
apply Not "True"  = "False"
apply Not "False" = "True"
apply Not s       = s
apply _ s = s


access2D :: CSVData -> ColRow -> Int -> ColRow -> Int -> String
access2D table COL colIdx ROW rowIdx = (transpose table !! (colIdx - 1)) !! (rowIdx - 1)
access2D table ROW rowIdx COL colIdx = (table !! (rowIdx - 1)) !! (colIdx - 1)


--------------------------------------------------------------------------------
-- Query handling

queryHandler :: VarName -> Query -> CSVState ()
queryHandler var query = do
  ctx <- get
  case query of

      
    Limit limit -> do
      let table = fromJust (Map.lookup var ctx)
      let limitedData = take limit table
      addToContext var limitedData


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
      let rowPairs = combineRows table1 table2
      let filteredRowPairs = filter (\(row1, row2) -> row1 !! (colNum - 1) == row2 !! (colNum - 1)) rowPairs
      let zippedRowPairs = map (uncurry zip) filteredRowPairs
      let mergedData = mergeRows mergeType zippedRowPairs
      addToContext var mergedData
      where
        mergeRows :: MergeType -> [[(String, String)]] -> [CSVRow]
        mergeRows LeftMerge  = map (map (\(p, q) -> if null p then q else p))
        mergeRows RightMerge = map (map (\(p, q) -> if null q then p else q))

    Filter filterQuery -> do
      filteredResult <- filterResult filterQuery
      addToContext var filteredResult
      where
        filterResult :: FilterQuery -> CSVState CSVData
        filterResult (FilterColRowIsNotNull (ColRowData table _ colNum)) = do
          let tableData = fromJust (Map.lookup table ctx)
          return $ filter (\row -> row !! (colNum - 1) /= "") tableData

        filterResult (FilterColRowIsNull (ColRowData table _ colNum)) = do
          let tableData = fromJust (Map.lookup table ctx)
          return $ filter (\row -> row !! (colNum - 1) == "") tableData

        filterResult (FilterColRow (ColRowData table _ colNum) operator (ColRowData _ _ colNum2)) = do
          let tableData = fromJust (Map.lookup table ctx)
          return $ filter (\row -> filterFunction operator (row !! (colNum - 1)) (row !! (colNum2 - 1))) tableData

        filterResult (FilterColRowOperand (ColRowData table _ colNum) operator operand) = do
          let tableData = fromJust (Map.lookup table ctx)
          return $ case operand of
            OperandLiteral literal ->
              filter (\row -> filterFunction operator (row !! (colNum - 1)) literal) tableData
            OperandNum num ->
              filter (\row -> filterFunction operator (read (row !! (colNum - 1)) :: Double) (fromIntegral num)) tableData
            OperandFloat float ->
              filter (\row -> filterFunction operator (read (row !! (colNum - 1)) :: Double) (realToFrac float)) tableData

        filterFunction :: (Ord a) => Operator -> a -> a -> Bool
        filterFunction op operand1 operand2 = case op of
          Equal              -> operand1 == operand2
          NotEqual           -> operand1 /= operand2
          LessThan           -> operand1 < operand2
          GreaterThan        -> operand1 > operand2
          LessThanOrEqual    -> operand1 <= operand2
          GreaterThanOrEqual -> operand1 >= operand2


    Zip vars -> do
      let tables = map (fromJust . (`Map.lookup` ctx)) vars  -- [[[CSVRow]]]
      let result = case tables of
            [tableA, tableB] -> zipWith (++) tableA tableB
            _                -> error "Zip expects exactly two input tables"
      addToContext var result

    -- Stack vars -> do
    --   let tables = map (fromJust . (`Map.lookup` ctx)) vars  -- [[[CSVRow]]]
      
    --   let result = case tables of
    --         [tableA, tableB] -> tableA ++ tableB
    --         _                -> error "Stack expects exactly two input tables"
        
    --   addToContext var result

    Stack vars -> do
      let [tableA, tableB] = map (fromJust . (`Map.lookup` ctx)) vars
      let result = tableA ++ tableB
          cleanedResult = filter (not . all null) result
      addToContext var cleanedResult

    Cross vars -> do
      let results = map (fromJust . (`Map.lookup` ctx)) vars
      let result = foldl (\acc row -> [r1 ++ r2 | r1 <- acc, r2 <- row]) [[]] results
      addToContext var result

    Get colRows -> do
      let columns = transpose $ map (\(ColRowData table _ colNum) ->
            let tableData = fromJust (Map.lookup table ctx)
             in map (!! (colNum - 1)) tableData) colRows
            
      let columns = transpose $ map (\(ColRowData table _ colNum) ->
            let tableData = fromJust (Map.lookup table ctx)
             in map (!! (colNum - 1)) tableData) colRows
      addToContext var columns

    Concat (ColRowData table _ colNum) literals -> do
      let tableData = fromJust (Map.lookup table ctx)
      let updatedData = map (\row -> insertAfter row colNum literals) tableData
      let replacedData = map (\row -> let colValue = row !! (colNum - 1) in replaceWith "$" colValue row) updatedData
      addToContext var replacedData

    