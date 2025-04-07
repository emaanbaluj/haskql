import CQLParser (parseCql)
import CQLexer (alexScanTokens)
import System.Environment (getArgs)

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let tokens = alexScanTokens contents
  let parsed = parseCql tokens
  print parsed
