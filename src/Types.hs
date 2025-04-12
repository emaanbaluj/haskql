module Types (CSVMap, CSVRow, CSVState, CSVData) where

import CQLParser (VarName)
import Control.Monad.State (StateT)
import Data.Map (Map)

type CSVData = [CSVRow]

type CSVMap = Map VarName CSVData

type CSVState = StateT CSVMap IO

type CSVRow = [String]