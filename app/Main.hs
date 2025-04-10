import CQLParser (parseCql)
import CQLexer (alexScanTokens)
import Control.Monad.State (runStateT)
import Eval (eval)
import State (initialContext)
import System.Environment (getArgs)

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let tokens = alexScanTokens contents
  let parsed = parseCql tokens
  (result, finalCtx) <- runStateT (mapM_ eval parsed) initialContext
  return ()
