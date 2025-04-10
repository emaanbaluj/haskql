module Eval (eval) where

import CQLParser (SortOrder (ASC, DESC), Stmt (..), Trim (TrimFalse, TrimTrue))
import Control.Monad.State
import qualified Data.Map as Map
import State (addToContext)
import Types (CSVData, CSVState)
import Util (convertToCSV, readCSV)

eval :: Stmt -> CSVState ()
eval (Import file var) = do
  result <- liftIO $ readCSV file
  addToContext var result
eval (Print var sort trim) = do
  ctx <- get
  let result = Map.lookup var ctx
  liftIO $ putStr $ formatResult result sort trim
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

-- TODO: Implement trimming
trimData :: CSVData -> Trim -> CSVData
trimData result TrimTrue = result
trimData result TrimFalse = result