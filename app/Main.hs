import CQLParser (Stmt (..), parseCql)
import CQLexer (alexScanTokens)
import System.Environment (getArgs)

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let tokens = alexScanTokens contents
  let parsed = parseCql tokens

  mapM_ statementPrinter parsed

statementPrinter :: Stmt -> IO ()
statementPrinter stmt = case stmt of
  Import file var -> do
    putStrLn $ "Import " ++ file ++ " as " ++ var
  Print var sort trim -> do
    putStrLn $ "Print " ++ var ++ " " ++ show sort ++ " " ++ show trim
  Write var file -> do
    putStrLn $ "Write " ++ var ++ " to " ++ file
  Set var query -> do
    putStrLn $ "Set " ++ var ++ " as " ++ show query
