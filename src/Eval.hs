module Eval (eval) where

import CQLParser (Query (Cross), SortOrder (ASC, DESC), Stmt (..), Trim (TrimFalse, TrimTrue))
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import State (addToContext)
import Types (CSVData, CSVState)
import Util (convertToCSV, readCSV, trimString)

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
    Cross [var1, var2] -> do
      let result1 = Map.lookup var1 ctx
      let result2 = Map.lookup var2 ctx
      let result = [row1 ++ row2 | row1 <- fromJust result1, row2 <- fromJust result2]
      addToContext var result
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