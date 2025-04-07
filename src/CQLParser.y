{
module CQLParser where
import CQLexer
}

%name parseCql
%tokentype { PosnToken }
%error { parseError }
%token
    sort     { PT _ (TokenSORT $$) }
    csvfilepath     { PT _ (TokenCSV $$) }
    colrow   { PT _ (TokenColRow _ _ _) }
    num      { PT _ (TokenNUM $$) }
    var      { PT _ (TokenVAR $$) }
    "AS"     { PT _ TokenAS }
    "IMPORT" { PT _ TokenIMPORT }
    "EXTRACT" { PT _ TokenEXTRACT }
    "SET" { PT _ TokenSET }
    "CROSS" { PT _ TokenCROSS }
    "TRIM" { PT _ TokenTRIM }
    "(" { PT _ TokenLPAREN }
    ")" { PT _ TokenRPAREN }
    "," { PT _ TokenCOMMA }
    ";" { PT _ TokenSEMICOLON }
    "=" { PT _ TokenEQUAL }
    "||" { PT _ TokenOR }
    "&&" { PT _ TokenAND }
    "==" { PT _ TokenEQ }
    "!=" { PT _ TokenNEQ }
    "<=" { PT _ TokenLE }
    ">=" { PT _ TokenGE }
    "<" { PT _ TokenLT }
    ">" { PT _ TokenGT }
    "+" { PT _ TokenPLUS }
    "-" { PT _ TokenMINUS }
    "*" { PT _ TokenMULT }
    "/" { PT _ TokenDIV }
    "--" { PT _ TokenCOMMENT }
    "FILTER" { PT _ TokenFILTER }
    "DISTINCT" { PT _ TokenDISTINCT }
    "GET" { PT _ TokenGET }
    "LIMIT" { PT _ TokenLIMIT }
    "REPLACE" { PT _ TokenREPLACE }
    "WITH" { PT _ TokenWITH }
    "REVERSE" { PT _ TokenREVERSE }
    "ARITY" { PT _ TokenARITY }
    "UNION" { PT _ TokenUNION }
    "INTERSECT" { PT _ TokenINTERSECT }
    "SUBTRACT" { PT _ TokenSUBTRACT }
    "WHEN" { PT _ TokenWHEN }
    "{" { PT _ TokenLBRACE }
    "}" { PT _ TokenRBRACE }
    "CONCAT" { PT _ TokenCONCAT }
    "COUNT" { PT _ TokenCOUNT }
    "WRITE" { PT _ TokenWRITE }
    "TO" { PT _ TokenTO }
    "FROM" { PT _ TokenFROM }
    "COL" { PT _ TokenCOL }
    "ROW" { PT _ TokenROW }
    "PRINT" { PT _ TokenPRINT }

%%
stmts :: { [ Stmt ] }
    : stmt stmts { $1 : $2 }
    | stmt { [$1] }

stmt :: { Stmt }
    : "IMPORT" csvfilepath "AS" var ";" { Import $2 $4 }
    | "PRINT" var ";" { Print $2 }
    | "WRITE" var "TO" csvfilepath ";" { Write $2 $4 }
  
    


{
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at end of file"
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok


data Stmt = 
     Import FilePath VarName
    | Print VarName
    | Write VarName FilePath
    deriving (Show, Eq)

type CSVFilePath = String
type VarName = String
}