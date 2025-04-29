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
    "AS"|"as"                       {\p _ -> PT p TokenAS }
    "SET"|"set"                     {\p _ -> PT p TokenSET }
    "CROSS"|"cross"                 {\p _ -> PT p TokenCROSS }
    "("                             {\p _ -> PT p TokenLPAREN }
    ")"                             {\p _ -> PT p TokenRPAREN }
    ","                             {\p _ -> PT p TokenCOMMA }
    ";"                             {\p _ -> PT p TokenSEMICOLON }
    "="                             {\p _ -> PT p TokenEQUAL }
    "||"                            {\p _ -> PT p TokenOR }
    "IS"|"is"                       {\p _ -> PT p TokenIS }
    "NOT"|"not"                     {\p _ -> PT p TokenNOT }
    "&&"                            {\p _ -> PT p TokenAND }
    "NULL"|"null"                   {\p _ -> PT p TokenNULL }
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
    "PRINT"|"print"                 {\p _ -> PT p TokenPRINT }
    "FILTER"|"filter"               {\p _ -> PT p TokenFILTER }
    "DISTINCT"|"distinct"           {\p _ -> PT p TokenDISTINCT }
    "GET"|"get"                     {\p _ -> PT p TokenGET }
    "LIMIT"|"limit"                 {\p _ -> PT p TokenLIMIT }
    "NOTRIM"|"notrim"               {\p _ -> PT p TokenNOTRIM }
    "LEFT_MERGE"|"left_merge"       {\p _ -> PT p TokenLEFTMERGE }
    "RIGHT_MERGE"|"right_merge"     {\p _ -> PT p TokenRIGHTMERGE }
    "ON"|"on"                       {\p _ -> PT p TokenON }
    "UNION"|"union"                 {\p _ -> PT p TokenUNION }
    "REPLACE"|"replace"             {\p _ -> PT p TokenREPLACE }
    "WITH"|"with"                   {\p _ -> PT p TokenWITH }
    "CONCAT"|"concat"               {\p _ -> PT p TokenCONCAT }
    "COUNT"|"count"                 {\p _ -> PT p TokenCOUNT }
    "WRITE"|"write"                 {\p _ -> PT p TokenWRITE } 
    "TO"|"to"                       {\p _ -> PT p TokenTO } 
    "IMPORT"|"import"               {\p _ -> PT p TokenIMPORT }
    "SORT"|"sort"                   {\p _ -> PT p TokenSORT }
    "ASC"|"asc"                     {\p _ -> PT p TokenASC }
    "DESC"|"desc"                   {\p _ -> PT p TokenDESC }
    "COL"|"col"                     {\p _ -> PT p TokenCOL }
    "ROW"|"row"                     {\p _ -> PT p TokenROW }
    "FROM"|"from"                   {\p _ -> PT p TokenFROM }
    "MAP"|"map"                     {\p _ -> PT p TokenMAP }
    "UPPER"|"upper"                 {\p _ -> PT p TokenUPPER }
    "LOWER"|"lower"                 {\p _ -> PT p TokenLOWER }
    "IN"|"in"                       {\p _ -> PT p TokenIN }
    "WHERE"|"where"                 {\p _ -> PT p TokenWHERE }
    "TRANSPOSE"|"transpose"         {\p _ -> PT p TokenTRANSPOSE}
    "ZIP"|"zip"                     {\p _ -> PT p  TokenZIP}
    "STACK"|"stack"                 {\p _ -> PT p  TokenSTACK}
    "NOSORT"|"nosort"               {\p _ -> PT p TokenNOSORT }
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
  | TokenFROM
  | TokenNOTRIM
  | TokenLEFTMERGE
  | TokenRIGHTMERGE
  | TokenON
  | TokenUNION
  | TokenREPLACE
  | TokenWITH
  | TokenCONCAT
  | TokenCOUNT
  | TokenWRITE
  | TokenTO
  | TokenIMPORT
  | TokenCOMMENT
  | TokenCOL
  | TokenROW
  | TokenMAP
  | TokenUPPER
  | TokenLOWER
  | TokenIN
  | TokenTRANSPOSE
  | TokenWHERE
  | TokenZIP
  | TokenSTACK
  | TokenNOSORT
  | TokenCSV String 
  | TokenLITERAL String
  | TokenVAR String
  | TokenNUM String
  | TokenFLOAT String
  deriving (Eq, Show)

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}