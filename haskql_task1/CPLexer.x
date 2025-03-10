{
module ToyLex where
}

%wrapper "posn"
$digit = [0-9]

tokens :-
    $white+                         ;
    
    "x"$digit*                      { \p s -> PT p (TokenVar s) } 
    $digit+                         { \p s -> PT p (TokenNat s) }
    
    "true"                          { \p s -> PT p (TokenTrue s) }
    "false"                         { \p s -> PT p (TokenFalse s) }
    
    \<                              { \p s -> PT p TokenLess }
    \+                              { \p s -> PT p TokenPlus }
    
    "if"                            { \p s -> PT p TokenIf }
    "then"                          { \p s -> PT p TokenThen }
    "else"                          { \p s -> PT p TokenElse }
    
    "let"                           { \p s -> PT p TokenLet }
    "in"                            { \p s -> PT p TokenIn }
    
    \:                              { \p s -> PT p TokenColon }  
    \\                              { \p s -> PT p TokenLam }    
    
    \=                              { \p s -> PT p TokenEq }  
    \.                              { \p s -> PT p TokenDot }
    
    \(                              { \p s -> PT p TokenLParen }
    \)                              { \p s -> PT p TokenRParen }
    
    \-\>                            { \p s -> PT p TokenArrow } 
    
    \N                             { \p s -> PT p TokenNatType }
    \B                             { \p s -> PT p TokenBoolType }
{
data PosnToken = PT AlexPosn Token 
  deriving (Eq, Show)

data Token = 
    TokenVar String
  | TokenNat String
  | TokenTrue String
  | TokenFalse String
  | TokenLess
  | TokenPlus
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenLet
  | TokenIn
  | TokenEq
  | TokenDot
  | TokenColon
  | TokenLam
  | TokenArrow
  | TokenLParen
  | TokenRParen
  | TokenNatType
  | TokenBoolType
  deriving (Eq, Show)

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}