{
module ToyParse where
import ToyLex
}

%name parseToy
%tokentype { PosnToken }
%error { parseError }
%token
    var     { PT _ (TokenVar $$) }
    nat     { PT _ (TokenNat $$) }
    true    { PT _ (TokenTrue $$) }
    false   { PT _ (TokenFalse $$) }
    '<'     { PT _ TokenLess }
    '+'     { PT _ TokenPlus }
    if      { PT _ TokenIf }
    then    { PT _ TokenThen }
    else    { PT _ TokenElse }
    let     { PT _ TokenLet }
    in      { PT _ TokenIn }
    
    ':'     { PT _ TokenColon }
    '\\'    { PT _ TokenLam }
    
    '='     { PT _ TokenEq }
    '.'     { PT _ TokenDot }
    
    '('     { PT _ TokenLParen }
    ')'     { PT _ TokenRParen }
    
    '->'    { PT _ TokenArrow }
    'N'     { PT _ TokenNatType }
    'B'     { PT _ TokenBoolType }




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

Exp : Exp Exp %prec APP               { App $1 $2 }
    | Exp '<' Exp                     { LessThan $1 $3 }
    | Exp '+' Exp                     { Add $1 $3 }
    | nat                             { ToyNat (read $1) }
    | true                            { ToyTrue }
    | false                           { ToyFalse }
    | var                             { Var $1 }
    | if Exp then Exp else Exp        { If $2 $4 $6 }
    | let '(' var ':' T ')' '=' Exp in Exp  { Let $3 $5 $8 $10 }
    | '\\' '(' var ':' T ')' '.' Exp  { LamExpr $3 $5 $8 }
    | '(' Exp ')'                     { $2 }


T   : 'N'                            { TNat }
    | 'B'                            { TBool }
    | T '->' T                       { TArrow $1 $3 }

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