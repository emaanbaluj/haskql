import CQLexer (alexScanTokens)
import CQLParser (parseCql)
import Eval (eval)
import System.Environment (getArgs)
import Control.Monad.State (evalStateT)
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "Arguments: " ++ show args
  
  [filename] <- getArgs
  putStrLn $ "Reading file: " ++ filename
  
  contents <- readFile filename
  putStrLn $ "File contents: " ++ contents
  
  let tokens = alexScanTokens contents
  putStrLn $ "Tokens: " ++ show tokens
  
  let ast = parseCql tokens
  putStrLn $ "AST: " ++ show ast
  
  evalStateT (mapM_ eval ast) Map.empty
  putStrLn "Execution complete"