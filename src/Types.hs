module Types
  ( CSVMap
  , CSVRow
  , CSVState
  , CSVData
  , BaseType(..)
  , ColumnType(..)
  , Type(..)
  , TypeEnv
  ) where


import CQLParser (VarName)
import Control.Monad.State (StateT)
import Data.Map (Map)


import qualified Data.Map as Map


data BaseType = TString | TInt | TBool
  deriving (Eq, Show)


data ColumnType = ColumnType String BaseType
  deriving (Eq, Show)


data Type
  = TypeTable [ColumnType]
  | TypeString
  | TypeInt
  | TypeBool
  | TypeUnknown
  deriving (Eq, Show)

type TypeEnv = Map.Map String Type

type CSVData = [CSVRow]

type CSVMap = Map VarName CSVData

type CSVState = StateT CSVMap IO

type CSVRow = [String]