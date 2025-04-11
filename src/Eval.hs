module Eval (eval) where

import CQLParser (ColRowData (..), FilterQuery (..), Query (..), SortOrder (..), Stmt (..), Trim (..))
import Control.Monad.State
import Data.List (transpose)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import State (addToContext)
import Types (CSVData, CSVState)
import Util (convertToCSV, insertAfter, readCSV, replaceWith, trimString)

eval :: Stmt -> CSVState ()
eval (Import file var) = do
  result <- liftIO $ readCSV file
  addToContext var result
eval (Print var sort trim) = do
  ctx <- get
  let result = Map.lookup var ctx
  liftIO $ putStr $ formatResult result sort trim
eval (Set var query) = do
  ctx <- get
  case query of
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
    _ -> return ()
eval _ = return ()

formatResult :: Maybe CSVData -> SortOrder -> Trim -> String
formatResult Nothing _ _ = "Variable not found"
formatResult (Just result) sort trim =
  let sortedData = sortData result sort
      trimmedData = trimData sortedData trim
   in convertToCSV trimmedData

-- TODO: Implement sorting
sortData :: CSVData -> SortOrder -> CSVData
sortData result ASC = result
sortData result DESC = result

trimData :: CSVData -> Trim -> CSVData
trimData result TrimTrue = map (map trimString) result
trimData result TrimFalse = result

filterResult :: FilterQuery -> CSVState CSVData
filterResult (FilterColRowIsNotNull (ColRowData table _ colNum)) = do
  ctx <- get
  let tableData = fromJust (Map.lookup table ctx)
  let filteredData = filter (\row -> row !! (colNum - 1) /= "") tableData
  return filteredData
filterResult _ = undefined