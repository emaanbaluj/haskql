{
module CQLexer where
}

%wrapper "posn"

$alpha = [a-zA-Z]
$digit = [0-9]

@comment = \-\-[^\n]*
@dollar = \$
@num = $digit+
@float = $digit+\.$digit+
@var = $alpha($alpha|$digit)*
@literal = \"[^\,\$]+\"|@dollar
@csvfilepath = \"([a-zA-Z0-9_\-\.\/\\]+)\.csv\"

tokens :-
    $white+                         ;
    @comment                        ;
    "AS"                            {\p _ -> PT p TokenAS }
    "SET"                           {\p _ -> PT p TokenSET }
    "CROSS"                         {\p _ -> PT p TokenCROSS }
    "("                             {\p _ -> PT p TokenLPAREN }
    ")"                             {\p _ -> PT p TokenRPAREN }
    ","                             {\p _ -> PT p TokenCOMMA }
    ";"                             {\p _ -> PT p TokenSEMICOLON }
    "="                             {\p _ -> PT p TokenEQUAL }
    "||"                            {\p _ -> PT p TokenOR }
    "IS"                            {\p _ -> PT p TokenIS }
    "NOT"                           {\p _ -> PT p TokenNOT }
    "&&"                            {\p _ -> PT p TokenAND }
    "NULL"                          {\p _ -> PT p TokenNULL }
    "=="                            {\p _ -> PT p TokenEQ }
    "!="                            {\p _ -> PT p TokenNEQ }
    "<="                            {\p _ -> PT p TokenLE }
    \"                              {\p _ -> PT p TokenSPEECH }
    ">="                            {\p _ -> PT p TokenGE }
    "<"                             {\p _ -> PT p TokenLT }
    "."                             {\p _ -> PT p TokenDOT }
    ">"                             {\p _ -> PT p TokenGT }
    "+"                             {\p _ -> PT p TokenPLUS }
    "-"                             {\p _ -> PT p TokenMINUS }
    "*"                             {\p _ -> PT p TokenMULT }
    "/"                             {\p _ -> PT p TokenDIV }
    "--"                            {\p _ -> PT p TokenCOMMENT }
    "PRINT"                         {\p _ -> PT p TokenPRINT }
    "FILTER"                        {\p _ -> PT p TokenFILTER }
    "DISTINCT"                      {\p _ -> PT p TokenDISTINCT }
    "GET"                           {\p _ -> PT p TokenGET }
    "LIMIT"                         {\p _ -> PT p TokenLIMIT }
    "NOTRIM"                        {\p _ -> PT p TokenNOTRIM }
    "LEFT_MERGE"                    {\p _ -> PT p TokenLEFTMERGE }
    "RIGHT_MERGE"                   {\p _ -> PT p TokenRIGHTMERGE }
    "ON"                            {\p _ -> PT p TokenON }
    "UNION"                         {\p _ -> PT p TokenUNION }
    "INTERSECT"                     {\p _ -> PT p TokenINTERSECT }
    "SUBTRACT"                      {\p _ -> PT p TokenSUBTRACT }
    "REPLACE"                       {\p _ -> PT p TokenREPLACE }
    "WITH"                          {\p _ -> PT p TokenWITH }
    "REVERSE"                       {\p _ -> PT p TokenREVERSE }
    ">>="                           {\p _ -> PT p TokenBIND }
    "THEN"                          {\p _ -> PT p TokenTHEN }
    "ARITY"                         {\p _ -> PT p TokenARITY }
    "WHEN"                          {\p _ -> PT p TokenWHEN }
    "ALL"                           {\p _ -> PT p TokenALL }
    "{"                             {\p _ -> PT p TokenLBRACE }
    "}"                             {\p _ -> PT p TokenRBRACE }
    "CONCAT"                        {\p _ -> PT p TokenCONCAT }
    "COUNT"                         {\p _ -> PT p TokenCOUNT }
    "WRITE"                         {\p _ -> PT p TokenWRITE } 
    "TO"                            {\p _ -> PT p TokenTO } 
    "IMPORT"                        {\p _ -> PT p TokenIMPORT }
    "FROM"                          {\p _ -> PT p TokenFROM }
    "SORT"                          {\p _ -> PT p TokenSORT }
    "ASC"                           {\p _ -> PT p TokenASC }
    "DESC"                          {\p _ -> PT p TokenDESC }
    "COL"                           {\p _ -> PT p TokenCOL }
    "ROW"                           {\p _ -> PT p TokenROW }
    "MAP"                           {\p _ -> PT p TokenMAP }
    "UPPER"                         {\p _ -> PT p TokenUPPER }
    "LOWER"                         {\p _ -> PT p TokenLOWER }
    "IN"                            {\p _ -> PT p TokenIN }
    "WHERE"                         {\p _ -> PT p TokenWHERE }
    "TRANSPOSE"                     {\p _ -> PT p TokenTRANSPOSE}
    @csvfilepath                    {\p s -> PT p (TokenCSV (init (tail s))) }
    @literal                        {\p s -> 
      if s == "$" 
        then PT p (TokenLITERAL s) 
        else PT p (TokenLITERAL (init (tail s)))
    }
    @var                            {\p s -> PT p (TokenVAR s) }
    @num                            {\p s -> PT p (TokenNUM s) }
    @float                          {\p s -> PT p (TokenFLOAT s) }
    
{
data PosnToken = PT AlexPosn Token 
  deriving (Eq, Show)

data Token = 
  TokenAS
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
  | TokenTHEN
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
  | TokenBIND
  | TokenCOUNT
  | TokenWRITE
  | TokenTO
  | TokenIMPORT
  | TokenFROM
  | TokenCOMMENT
  | TokenCOL
  | TokenROW
  | TokenMAP
  | TokenUPPER
  | TokenLOWER
  | TokenIN
  | TokenTRANSPOSE
  | TokenWHERE
  | TokenCSV String 
  | TokenLITERAL String
  | TokenVAR String
  | TokenNUM String
  | TokenFLOAT String
  deriving (Eq, Show)

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}