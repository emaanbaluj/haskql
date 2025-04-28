import CQLParser (parseCql)
import CQLexer (alexScanTokens)
import Control.Monad.State (runStateT)
import qualified Data.Map as Map
import Eval (eval)
import System.Environment (getArgs)
import TypeChecker (typecheck)

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let tokens = alexScanTokens contents
  let parsed = parseCql tokens
  (_, _) <- runStateT (mapM_ typecheck parsed) Map.empty
  (_, _) <- runStateT (mapM_ eval parsed) Map.empty
  return ()
