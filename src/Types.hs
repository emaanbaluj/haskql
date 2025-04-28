module Types
  ( CSVMap,
    CSVRow,
    CSVState,
    CSVData,
    BaseType (..),
    ColumnType (..),
  )
where

import CQLParser (VarName)
import Control.Monad.State (StateT)
import Data.Map (Map)

data BaseType = TString | TInt | TBool
  deriving (Eq, Show)

data ColumnType = ColumnType String BaseType
  deriving (Eq, Show)

type CSVData = [CSVRow]

type CSVMap = Map VarName CSVData

type CSVState = StateT CSVMap IO

type CSVRow = [String]