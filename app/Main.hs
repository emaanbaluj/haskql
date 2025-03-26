import CPLexer (alexScanTokens)  
import CPParser (parseCP)        
import System.Environment (getArgs)  
import System.IO (readFile, writeFile) 
import Data.List (intercalate)  

main :: IO ()
main = do  
    args <- getArgs  
    print args
        

