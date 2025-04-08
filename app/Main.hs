import CQLParser (parseCql)
import CQLexer (alexScanTokens)
import System.Environment (getArgs)
import Util (statementPrinter)

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let tokens = alexScanTokens contents
  let parsed = parseCql tokens

  mapM_ statementPrinter parsed
