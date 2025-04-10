module Types (CSVMap, CSVState, CSVData) where

import CQLParser (VarName)
import Control.Monad.State (StateT)
import Data.Map (Map)

type CSVData = [[String]]

type CSVMap = Map VarName CSVData

type CSVState = StateT CSVMap IO