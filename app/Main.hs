-- Import required modules
import CPLexer (alexScanTokens)  
import CPParser (parseCP)        
import System.Environment (getArgs)  
import System.IO (readFile, writeFile) 
import Data.List (intercalate)  

-- Main function: Entry point of the interpreter
main :: IO ()
main = do  
    args <- getArgs  -- Get command-line arguments (e.g., ["cp A.csv B.csv"])
    case args of
        -- Expect exactly one argument (a single query string)
        [query] -> do  
            let tokens = alexScanTokens query  -- Convert query into tokens using the lexer
            case parseCP tokens of  -- Convert tokens into an Abstract Syntax Tree (AST) using the parser
                ("cp", fileA, fileB) -> do
                    putStrLn $ "Processing CSV files: " ++ fileA ++ " and " ++ fileB
                    processCP fileA fileB  -- Compute the Cartesian product of the two CSV files
                _ -> putStrLn "Invalid query!"  
        _ -> putStrLn "Usage: ./cpCommand \"cp fileA.csv fileB.csv\""  
        

-- Function to process Cartesian Product (cp) operation
processCP :: FilePath -> FilePath -> IO()
processCP csv1 csv2 = do
    contentA <- readFile csv1  -- Read the first CSV file
    contentB <- readFile csv2  -- Read the second CSV file

    let lines1 = lines contentA  -- Split file A into lines (each line is a row)
        lines2 = lines contentB  -- Split file B into lines
        rows1 = map (splitOn ',') lines1  -- Split each row of A into a list of values
        rows2 = map (splitOn ',') lines2  -- Split each row of B into a list of values
        cartesianProduct = [a ++ b | a <- rows1, b <- rows2]  -- Compute Cartesian product (concatenate each row of A with each row of B)
        outputLines = map (intercalate ",") cartesianProduct  -- Convert list of lists back into CSV format

    writeFile "output.csv" (unlines outputLines)  -- Write the result to output.csv
    putStrLn "Cartesian Product written to output.csv"  

-- Function to split a string on a given delimiter (e.g., split on ',')
splitOn :: Char -> String -> [String]
splitOn delimitter input =
    case break (== delimitter) input of
        (x, _ : xs) -> x : splitOn delimitter xs  -- Recursively split remaining string
        (x, _)      -> [x]  -- If there's no more delimiter, return the last part