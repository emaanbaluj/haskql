module State (addToContext, CSVState) where

import CQLParser (VarName)
import Control.Monad.State (MonadState (get, put))
import qualified Data.Map as Map
import Types (CSVData, CSVState)

addToContext :: VarName -> CSVData -> CSVState ()
addToContext variable csvData = do
  ctx <- get
  put (Map.insert variable csvData ctx)
