module State (addToContext, CSVData, CSVState, initialContext) where

import CQLParser (VarName)
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

type CSVData = Map VarName [[String]]

type CSVState = StateT CSVData IO

initialContext :: CSVData
initialContext = Map.empty

addToContext :: VarName -> [[String]] -> CSVState ()
addToContext variable csvData = do
  ctx <- get
  put (Map.insert variable csvData ctx)
