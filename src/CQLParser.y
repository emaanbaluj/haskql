{
module CQLParser where
import CQLexer
}

%name parseCql
%tokentype { PosnToken }
%error { parseError }
%token
    as { PT _ TokenAS }
    extract { PT _ TokenEXTRACT }
    set { PT _ TokenSET }
    cross { PT _ TokenCROSS }
    sort { PT _ TokenSORT }
    trim { PT _ TokenTRIM }
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
    replace { PT _ TokenREPLACE }
    when { PT _ TokenWHEN }
    "{" { PT _ TokenLBRACE }
    "}" { PT _ TokenRBRACE }
    "CONCAT" { PT _ TokenCONCAT }
    "COUNT" { PT _ TokenCOUNT }
    "WRITE" { PT _ TokenWRITE }
    "TO" { PT _ TokenTO }
    "IMPORT" { PT _ TokenIMPORT }
    "FROM" { PT _ TokenFROM }
    filename { PT _ (TokenFILENAME $$) }
    var      { PT _ (TokenVAR $$) }
    sort     { PT _ (TokenSORT $$) }
    col

-- associativity rules
%nonassoc 'N' 'B' true false nat var '(' ')'

%right '.' '\\' '='
%right in
%left let

%right if
%right then  
%right else
%right '+'
%right '<'
%right ':'
%right '->'
%left APP

%%
ExpList : 'IMPORT' file 'FROM' filepath ';' ExpList { Import $2 $4 }
        | 'PRINT' var ';' ExpList { Sequence (Print $2) $3 }
        | 'WRITE' var 'TO' filepath ';' { Write $2 $4 }
        | Exp { $1 }
        | Exp ';' ExpList { Sequence $1 $3 }


Exp : 'SET' var  'AS' '(' Query ')' ';'   { SET $2 $5 }
    | 

Query : ...


{
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at end of file"
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok


data Exp = 
    | Import String String
    | Print String
    | Write String String
    | Set String Query
    deriving (Show, Eq)
}