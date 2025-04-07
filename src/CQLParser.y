{
module CQLParser where
import CQLexer
}

%name parseCql
%tokentype { PosnToken }
%error { parseError }
%token
    sort     { PT _ (TokenSORT $$) }
    filepath     { PT _ (TokenCSV $$) }
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
    filter { PT _ TokenFILTER }
    distinct { PT _ TokenDISTINCT }
    get { PT _ TokenGET }
    limit { PT _ TokenLIMIT }
    replace { PT _ TokenREPLACE }
    with { PT _ TokenWITH }
    reverse { PT _ TokenREVERSE }
    arity { PT _ TokenARITY }
    union { PT _ TokenUNION }
    intersect { PT _ TokenINTERSECT }
    subtract { PT _ TokenSUBTRACT }
    when { PT _ TokenWHEN }
    "{" { PT _ TokenLBRACE }
    "}" { PT _ TokenRBRACE }
    concat { PT _ TokenCONCAT }
    count { PT _ TokenCOUNT }
    write { PT _ TokenWRITE }
    to   { PT _ TokenTO }
    from     { PT _ TokenFROM }
    col      { PT _ TokenCOL }
    row      { PT _ TokenROW }

%%
ExpList : "IMPORT" filepath "AS" var ";" { Import $2 $4 }
        -- | 'PRINT' var ';' ExpList { Sequence (Print $2) $3 }
        -- | 'WRITE' var 'TO' filepath ';' { Write $2 $4 }
        -- | Exp { $1 }
        -- | Exp ';' ExpList { Sequence $1 $3 }


-- Exp : 'SET' var  'AS' '(' Query ')' ';'   { SET $2 $5 }
--     | 

-- Query : ...


{
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at end of file"
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok


data Exp = 
     Import String String
    -- | Print String
    -- | Write String String
    -- | Set String Query
    deriving (Show, Eq)
}