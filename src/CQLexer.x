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
@literal = \"[^\,\$\"]*\"|@dollar
@csvfilepath = \"([a-zA-Z0-9_\-\.\/\\]+)\.csv\"

-- Case insensitive keyword macros
@as = [aA][sS]
@set = [sS][eE][tT]
@cross = [cC][rR][oO][sS][sS]
@is = [iI][sS]
@not = [nN][oO][tT]
@null = [nN][uU][lL][lL]
@print = [pP][rR][iI][nN][tT]
@filter = [fF][iI][lL][tT][eE][rR]
@distinct = [dD][iI][sS][tT][iI][nN][cC][tT]
@get = [gG][eE][tT]
@limit = [lL][iI][mM][iI][tT]
@notrim = [nN][oO][tT][rR][iI][mM]
@leftmerge = [lL][eE][fF][tT]_[mM][eE][rR][gG][eE]
@rightmerge = [rR][iI][gG][hH][tT]_[mM][eE][rR][gG][eE]
@on = [oO][nN]
@union = [uU][nN][iI][oO][nN]
@replace = [rR][eE][pP][lL][aA][cC][eE]
@with = [wW][iI][tT][hH]
@concat = [cC][oO][nN][cC][aA][tT]
@count = [cC][oO][uU][nN][tT]
@write = [wW][rR][iI][tT][eE]
@to = [tT][oO]
@import = [iI][mM][pP][oO][rR][tT]
@sort = [sS][oO][rR][tT]
@asc = [aA][sS][cC]
@desc = [dD][eE][sS][cC]
@col = [cC][oO][lL]
@row = [rR][oO][wW]
@from = [fF][rR][oO][mM]
@map = [mM][aA][pP]
@upper = [uU][pP][pP][eE][rR]
@lower = [lL][oO][wW][eE][rR]
@in = [iI][nN]
@where = [wW][hH][eE][rR][eE]
@transpose = [tT][rR][aA][nN][sS][pP][oO][sS][eE]
@zip = [zZ][iI][pP]
@stack = [sS][tT][aA][cC][kK]
@nosort = [nN][oO][sS][oO][rR][tT]
@insert = [iI][nN][sS][eE][rR][tT]
@reverse = [rR][eE][vV][eE][rR][sS][eE]
@arity = [aA][rR][iI][tT][yY]
@update = [uU][pP][dD][aA][tT][eE]

tokens :-
    $white+                        ;
    @comment                       ;
    "("                            {\p _ -> PT p TokenLPAREN }
    ")"                            {\p _ -> PT p TokenRPAREN }
    ","                            {\p _ -> PT p TokenCOMMA }
    ";"                            {\p _ -> PT p TokenSEMICOLON }
    "=="                           {\p _ -> PT p TokenEQ }
    "!="                           {\p _ -> PT p TokenNEQ }
    "<="                           {\p _ -> PT p TokenLE }
    \"                             {\p _ -> PT p TokenSPEECH }
    ">="                           {\p _ -> PT p TokenGE }
    "<"                            {\p _ -> PT p TokenLT }
    "."                            {\p _ -> PT p TokenDOT }
    ">"                            {\p _ -> PT p TokenGT }
    "+"                            {\p _ -> PT p TokenPLUS }
    "-"                            {\p _ -> PT p TokenMINUS }
    @update                        {\p _ -> PT p TokenUPDATE }
    @insert                        {\p _ -> PT p TokenINSERT }
    @as                            {\p _ -> PT p TokenAS }
    @set                           {\p _ -> PT p TokenSET }
    @cross                         {\p _ -> PT p TokenCROSS }
    @is                            {\p _ -> PT p TokenIS }
    @not                           {\p _ -> PT p TokenNOT }
    @null                          {\p _ -> PT p TokenNULL }
    @print                         {\p _ -> PT p TokenPRINT }
    @filter                        {\p _ -> PT p TokenFILTER }
    @distinct                      {\p _ -> PT p TokenDISTINCT }
    @get                           {\p _ -> PT p TokenGET }
    @limit                         {\p _ -> PT p TokenLIMIT }
    @notrim                        {\p _ -> PT p TokenNOTRIM }
    @leftmerge                     {\p _ -> PT p TokenLEFTMERGE }
    @rightmerge                    {\p _ -> PT p TokenRIGHTMERGE }
    @on                            {\p _ -> PT p TokenON }
    @union                         {\p _ -> PT p TokenUNION }
    @replace                       {\p _ -> PT p TokenREPLACE }
    @with                          {\p _ -> PT p TokenWITH }
    @concat                        {\p _ -> PT p TokenCONCAT }
    @write                         {\p _ -> PT p TokenWRITE } 
    @to                            {\p _ -> PT p TokenTO } 
    @import                        {\p _ -> PT p TokenIMPORT }
    @sort                          {\p _ -> PT p TokenSORT }
    @asc                           {\p _ -> PT p TokenASC }
    @desc                          {\p _ -> PT p TokenDESC }
    @col                           {\p _ -> PT p TokenCOL }
    @row                           {\p _ -> PT p TokenROW }
    @from                          {\p _ -> PT p TokenFROM }
    @map                           {\p _ -> PT p TokenMAP }
    @upper                         {\p _ -> PT p TokenUPPER }
    @lower                         {\p _ -> PT p TokenLOWER }
    @in                            {\p _ -> PT p TokenIN }
    @where                         {\p _ -> PT p TokenWHERE }
    @transpose                     {\p _ -> PT p TokenTRANSPOSE}
    @zip                           {\p _ -> PT p TokenZIP}
    @stack                         {\p _ -> PT p TokenSTACK}
    @nosort                        {\p _ -> PT p TokenNOSORT }
    @reverse                       {\p _ -> PT p TokenREVERSE}
    @arity                         {\p _ -> PT p TokenARITY}
    @csvfilepath                   {\p s -> PT p (TokenCSV (init (tail s))) }
    @literal                       {\p s -> 
      if s == "$" 
        then PT p (TokenLITERAL s) 
        else PT p (TokenLITERAL (init (tail s)))
    }
    @var                           {\p s -> PT p (TokenVAR s) }
    @num                           {\p s -> PT p (TokenNUM s) }
    @float                         {\p s -> PT p (TokenFLOAT s) }
    
{
data PosnToken = PT AlexPosn Token 
  deriving (Eq, Show)

data Token = 
  TokenAS
  | TokenSET
  | TokenCROSS
  | TokenLPAREN
  | TokenRPAREN
  | TokenUPDATE
  | TokenCOMMA
  | TokenSEMICOLON
  | TokenDOT
  | TokenEQ
  | TokenNULL
  | TokenNEQ
  | TokenLE
  | TokenSPEECH
  | TokenINSERT
  | TokenGE
  | TokenLT
  | TokenGT
  | TokenPLUS
  | TokenMINUS
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
  | TokenREVERSE
  | TokenARITY
  | TokenCSV String 
  | TokenLITERAL String
  | TokenVAR String
  | TokenNUM String
  | TokenFLOAT String
  deriving (Eq, Show)

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}