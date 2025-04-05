{
module CQLexer where
}

%wrapper "posn"
$digit = [0-9]
$alpha = [a-zA-Z]
$sortorder = [ASC|DESC]
$colrow = [COL|ROW]

tokens :-
    $white+                         ;
    "AS"                            {\p s -> PT p TokenAS }
    "EXTRACT"                       {\p s -> PT p TokenEXTRACT }
    "SET"                           {\p s -> PT p TokenSET }
    "CROSS"                         {\p s -> PT p TokenCROSS }
    "("                             {\p s -> PT p TokenLPAREN }
    ")"                             {\p s -> PT p TokenRPAREN }
    ","                             {\p s -> PT p TokenCOMMA }
    ";"                             {\p s -> PT p TokenSEMICOLON }
    "="                             {\p s -> PT p TokenEQUAL }
    "||"                            {\p s -> PT p TokenOR }
    "&&"                            {\p s -> PT p TokenAND }
    "NULL"                          {\p s -> PT p TokenNULL }
    "=="                            {\p s -> PT p TokenEQ }
    "!="                            {\p s -> PT p TokenNEQ }
    "<="                            {\p s -> PT p TokenLE }
    \"                              {\p s -> PT p TokenSPEECH }
    ">="                            {\p s -> PT p TokenGE }
    "<"                             {\p s -> PT p TokenLT }
    ">"                             {\p s -> PT p TokenGT }
    "+"                             {\p s -> PT p TokenPLUS }
    "-"                             {\p s -> PT p TokenMINUS }
    "*"                             {\p s -> PT p TokenMULT }
    "/"                             {\p s -> PT p TokenDIV }
    "--"                            {\p s -> PT p TokenCOMMENT }
    "PRINT"                         {\p s -> PT p TokenPRINT }
    "FILTER"                        {\p s -> PT p TokenFILTER }
    "DISTINCT"                      {\p s -> PT p TokenDISTINCT }
    "GET"                           {\p s -> PT p TokenGET }
    "LIMIT"                         {\p s -> PT p TokenLIMIT }
    "TRIM"                          {\p s -> PT p TokenTRIM }
    "LEFT_MERGE"                    {\p s -> PT p TokenLEFTMERGE }
    "RIGHT_MERGE"                   {\p s -> PT p TokenRIGHTMERGE }
    "ON"                            {\p s -> PT p TokenON }
    "UNION"                         {\p s -> PT p TokenUNION }
    "INTERSECT"                     {\p s -> PT p TokenINTERSECT }
    "SUBTRACT"                      {\p s -> PT p TokenSUBTRACT }
    "REPLACE"                       {\p s -> PT p TokenREPLACE }
    "WITH"                          {\p s -> PT p TokenWITH }
    "REVERSE"                       {\p s -> PT p TokenREVERSE }
    "ARITY"                         {\p s -> PT p TokenARITY }
    "WHEN"                          {\p s -> PT p TokenWHEN }
    "{"                             {\p s -> PT p TokenLBRACE }
    "}"                             {\p s -> PT p TokenRBRACE }
    "CONCAT"                        {\p s -> PT p TokenCONCAT }
    "COUNT"                         {\p s -> PT p TokenCOUNT }
    "WRITE"                         {\p s -> PT p TokenWRITE } 
    "TO"                            {\p s -> PT p TokenTO } 
    "IMPORT"                        {\p s -> PT p TokenIMPORT }
    "FROM"                          {\p s -> PT p TokenFROM }
    $alpha".csv"                    {\p s -> PT p (TokenCSV s) }
    $alpha                          {\p s -> PT p (TokenVAR s) }     
    $alpha"."$colrow"("$digit+")" { \p s -> 
        let (var, '$':num) = break (== '$') s
        in PT p (TokenVarColumn (init var) num COLTYPE) 
    }
    "SORT("$sortorder")"                {\p s -> 
      let (order:")") = break (== '(') s
      in PT p (TokenSORT order)
    }
{
data PosnToken = PT AlexPosn Token 
  deriving (Eq, Show)

data Token = 
  TokenAS
  | TokenEXTRACT
  | TokenSET
  | TokenCROSS
  | TokenLPAREN
  | TokenRPAREN
  | TokenCOMMA
  | TokenSEMICOLON
  | TokenEQUAL
  | TokenOR
  | TokenAND
  | TokenNULL
  | TokenEQ
  | TokenNEQ
  | TokenLE
  | TokenSPEECH
  | TokenGE
  | TokenLT
  | TokenGT
  | TokenPLUS
  | TokenMINUS
  | TokenMULT
  | TokenDIV
  | TokenPRINT
  | TokenFILTER
  | TokenDISTINCT
  | TokenGET
  | TokenSORT 
  | TokenASC
  | TokenDESC
  | TokenLIMIT
  | TokenTRIM
  | TokenLEFTMERGE
  | TokenRIGHTMERGE
  | TokenON
  | TokenUNION
  | TokenINTERSECT
  | TokenSUBTRACT
  | TokenREPLACE
  | TokenWITH
  | TokenREVERSE
  | TokenARITY
  | TokenWHEN
  | TokenLBRACE
  | TokenRBRACE
  | TokenCONCAT
  | TokenCOUNT
  | TokenWRITE
  | TokenTO
  | TokenIMPORT
  | TokenFROM
  | TokenCSV String 
  | TokenVAR String
  | TokenVarColumn String String
  | TokenCOMMENT
  deriving (Eq, Show)

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}