import CQLParser (parseCql)
import CQLexer (alexScanTokens)
import Control.Monad.State (execStateT)
import qualified Data.Map as Map
import Eval (eval)
import System.Environment (getArgs)
import TypeChecker (typecheck)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [filename] -> runCql filename
    [filename, "--typecheck=false"] -> runCql filename
    [filename, "--typecheck=true"] -> runCqlWithTypeCheck filename
    [filename, "-t"] -> runCqlWithTypeCheck filename
    _ -> putStrLn "Usage: stack run -- <filename> [--typecheck=true|false] | [-t]"

  return ()

runCql :: FilePath -> IO ()
runCql filename = do
  contents <- readFile filename
  let tokens = alexScanTokens contents
  let parsed = parseCql tokens

  _ <- execStateT (mapM_ eval parsed) Map.empty
  return ()

runCqlWithTypeCheck :: FilePath -> IO ()
runCqlWithTypeCheck filename = do
  contents <- readFile filename
  let tokens = alexScanTokens contents
  let parsed = parseCql tokens

  state <- execStateT (mapM_ typecheck parsed) Map.empty
  putStrLn $ "Type checking complete: " ++ show state
  _ <- execStateT (mapM_ eval parsed) Map.empty
  return ()