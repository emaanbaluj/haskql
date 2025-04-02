{
module CQLParser where
import CQLexer
}

%name parseCql
%tokentype { PosnToken }
%error { parseError }
%token
    var     { PT _ (TokenVar $$) }
    nat     { PT _ (TokenNat $$) }
    true    { PT _ (TokenTrue $$) }
    false   { PT _ (TokenFalse $$) }
    '<'     { PT _ TokenLess }
    
    -- tokens...




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
ExpList : 'IMPORT' file 'FROM' filepath ';' ExpList
        | 'PRINT' var ';' ExpList
        | 'WRITE' var 'TO' filepath ';' -- output.csv
        | Exp
        | Exp ';' ExpList


Exp : 'SET' var  'AS' '(' Query ')' ';'   { SET $2 $5 }
    | 

Query : ...

-- T   : 'N'                            { TNat }
--     | 'B'                            { TBool }
--     | T '->' T                       { TArrow $1 $3 }

{
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at end of file"
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok


data Exp = ToyNat Int
         | ToyTrue
         | ToyFalse
         | LessThan Exp Exp
         | Var String
         | Let String T Exp Exp
         | If Exp Exp Exp
         | Add Exp Exp 
         | App Exp Exp
         | LamExpr String T Exp
         deriving (Show, Eq)

data T =  TNat
        | TBool
        | TArrow T T
    deriving (Show, Eq)

}