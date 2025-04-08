{
module CQLexer where
}

%wrapper "posn"

$alpha = [a-zA-Z]
$digit = [0-9]

@comment = \-\-[^\n]*
@dollar = \$
@num = $digit+
@var = $alpha($alpha|$digit)*
@literal = \"[a-zA-Z0-9]+\"|@dollar
@csvfilepath = \"([a-zA-Z0-9_\-\.\/\\]+)\.csv\"

tokens :-
    $white+                         ;
    @comment                        ;
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
    "IS"                            {\p s -> PT p TokenIS }
    "NOT"                           {\p s -> PT p TokenNOT }
    "&&"                            {\p s -> PT p TokenAND }
    "NULL"                          {\p s -> PT p TokenNULL }
    "=="                            {\p s -> PT p TokenEQ }
    "!="                            {\p s -> PT p TokenNEQ }
    "<="                            {\p s -> PT p TokenLE }
    \"                              {\p s -> PT p TokenSPEECH }
    ">="                            {\p s -> PT p TokenGE }
    "<"                             {\p s -> PT p TokenLT }
    "."                             {\p s -> PT p TokenDOT }
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
    "NOTRIM"                        {\p s -> PT p TokenNOTRIM }
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
    "ALL"                           {\p s -> PT p TokenALL }
    "{"                             {\p s -> PT p TokenLBRACE }
    "}"                             {\p s -> PT p TokenRBRACE }
    "CONCAT"                        {\p s -> PT p TokenCONCAT }
    "COUNT"                         {\p s -> PT p TokenCOUNT }
    "WRITE"                         {\p s -> PT p TokenWRITE } 
    "TO"                            {\p s -> PT p TokenTO } 
    "IMPORT"                        {\p s -> PT p TokenIMPORT }
    "FROM"                          {\p s -> PT p TokenFROM }
    "SORT"                          {\p s -> PT p TokenSORT }
    "ASC"                           {\p s -> PT p TokenASC }
    "DESC"                          {\p s -> PT p TokenDESC }
    "COL"                           {\p s -> PT p TokenCOL }
    "ROW"                           {\p s -> PT p TokenROW }
    @csvfilepath                    {\p s -> PT p (TokenCSV (init (tail s))) }
    @literal                        {\p s -> 
      if s == "$" 
        then PT p (TokenLITERAL s) 
        else PT p (TokenLITERAL (init (tail s)))
    }
    @var                            {\p s -> PT p (TokenVAR s) }
    @num                            {\p s -> PT p (TokenNUM s) }
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
  | TokenDOT
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
  | TokenIS
  | TokenNOT
  | TokenASC
  | TokenDESC
  | TokenLIMIT
  | TokenNOTRIM
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
  | TokenALL
  | TokenLBRACE
  | TokenRBRACE
  | TokenCONCAT
  | TokenCOUNT
  | TokenWRITE
  | TokenTO
  | TokenIMPORT
  | TokenFROM
  | TokenCOMMENT
  | TokenCOL
  | TokenROW
  | TokenCSV String 
  | TokenLITERAL String
  | TokenVAR String
  | TokenNUM String
  deriving (Eq, Show)

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}