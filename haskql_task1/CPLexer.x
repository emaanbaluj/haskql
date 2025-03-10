{
module ToyLex where
}

%wrapper "posn"
$digit = [0-9]
$alpha = [a-zA-z]

tokens :-
    $white+                         ;

    "cp"                             {\p s -> PT p TokenCP }
    [a-zA-Z0-9\_\-]+".csv"                          {\p s -> PT p (TokenCSV s) }
   
{
data PosnToken = PT AlexPosn Token 
  deriving (Eq, Show)

data Token = 
    TokenCP
  | TokenCSV String 
  deriving (Eq, Show)

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}