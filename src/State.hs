module State (addToContext, CSVState) where

import CQLParser (VarName)
import Control.Monad.State
import qualified Data.Map as Map
import Types (CSVData, CSVMap, CSVState)

addToContext :: VarName -> CSVData -> CSVState ()
addToContext variable csvData = do
  ctx <- get
  put (Map.insert variable csvData ctx)
