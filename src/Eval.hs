module Eval (eval) where

import CQLParser (Stmt (..))
import Control.Monad.State (MonadIO (liftIO))
import State (addToContext)
import Types (CSVState)
import Util (readCSV)

eval :: Stmt -> CSVState ()
eval (Import file var) = do
  result <- liftIO $ readCSV file
  addToContext var result
  liftIO $ putStrLn $ "Imported " ++ file ++ " as " ++ var
eval _ = return ()