{
module CQLParser where
import CQLexer
}

%name parseCql
%tokentype { PosnToken }
%error { parseError }
%token
    csvfilepath     { PT _ (TokenCSV $$) }
    num      { PT _ (TokenNUM $$) }
    float      { PT _ (TokenFLOAT $$) }
    literal      { PT _ (TokenLITERAL $$) }
    var      { PT _ (TokenVAR $$) }
    "SORT"     { PT _ TokenSORT }
    "AS"     { PT _ TokenAS }
    "IMPORT" { PT _ TokenIMPORT }
    "EXTRACT" { PT _ TokenEXTRACT }
    "SET" { PT _ TokenSET }
    "CROSS" { PT _ TokenCROSS }
    "NOTRIM" { PT _ TokenNOTRIM }
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
    "." { PT _ TokenDOT }
    "FILTER" { PT _ TokenFILTER }
    "DISTINCT" { PT _ TokenDISTINCT }
    "GET" { PT _ TokenGET }
    "LIMIT" { PT _ TokenLIMIT }
    "REPLACE" { PT _ TokenREPLACE }
    "WITH" { PT _ TokenWITH }
    "REVERSE" { PT _ TokenREVERSE }
    "ARITY" { PT _ TokenARITY }
    "UNION" { PT _ TokenUNION }
    "INTERSECT" { PT _ TokenINTERSECT }
    "SUBTRACT" { PT _ TokenSUBTRACT }
    "WHEN" { PT _ TokenWHEN }
    "{" { PT _ TokenLBRACE }
    "}" { PT _ TokenRBRACE }
    "CONCAT" { PT _ TokenCONCAT }
    "COUNT" { PT _ TokenCOUNT }
    "WRITE" { PT _ TokenWRITE }
    "TO" { PT _ TokenTO }
    "FROM" { PT _ TokenFROM }
    "COL" { PT _ TokenCOL }
    "ROW" { PT _ TokenROW }
    "PRINT" { PT _ TokenPRINT }
    ">>=" { PT _ TokenBIND }
    "ASC" { PT _ TokenASC }
    "DESC" { PT _ TokenDESC }
    "ALL" { PT _ TokenALL }
    "LEFT_MERGE" { PT _ TokenLEFTMERGE }
    "RIGHT_MERGE" { PT _ TokenRIGHTMERGE }
    "ON" { PT _ TokenON }
    "IS" { PT _ TokenIS }
    "NOT" { PT _ TokenNOT }
    "NULL" { PT _ TokenNULL }
%%
stmts :: { [ Stmt ] }
    : stmt stmts { $1 : $2 }
    | stmt { [$1] }

stmt :: { Stmt }
    : "IMPORT" csvfilepath "AS" var ";" { Import $2 $4 }
    | "SET" var "AS" "(" queries ")" ";" { Set $2 $5 }
    | "SET" var "AS" queries ";" { Set $2 $4 }
    | "WRITE" var "TO" csvfilepath ";" { Write $2 $4 }
    | "PRINT" var sort trim ";" { Print $2 $3 $4 }

queries :: { [Query] }
    : "(" query ")" ">>=" queries { $2 : $5 }
    | "(" query ")" { [$2] }

query :: { Query }
    : "GET" colrows { Get $2 }
    | "CROSS" "(" vars ")" { Cross $3 }
    | "FILTER" filterquery { Filter $2 }
    | "DISTINCT" var { Distinct $2 }
    | mergetype "(" var "," var ")" "ON" "COL" num { Merge $1 $3 $5 (read $9) }
    | "UNION" "(" vars ")" { Union $3 }
    | "LIMIT" num { Limit (read $2) }
    | "CONCAT" colrow "WITH" "{" literals "}" { Concat $2 $5 }

mergetype :: { MergeType }
    : "LEFT_MERGE" { LeftMerge }
    | "RIGHT_MERGE" { RightMerge }

filterquery :: { FilterQuery }
    : colrow operator colrow { FilterColRow $1 $2 $3 }
    | colrow operator operand { FilterColRowOperand $1 $2 $3 }
    | colrow "IS" "NULL" { FilterColRowIsNull $1 }
    | colrow "IS" "NOT" "NULL" { FilterColRowIsNotNull $1 }

operand :: { Operand }
    : num { OperandNum (read $1) }
    | float { OperandFloat (read $1) }
    | literal { OperandLiteral $1 }

operator :: { Operator }
    : "==" { Equal }
    | "!=" { NotEqual }
    | "<=" { LessThanOrEqual }
    | ">=" { GreaterThanOrEqual }
    | "<" { LessThan }
    | ">" { GreaterThan }

sort :: { SortOrder }
    : "SORT" { ASC }
    | "SORT" "(" "ASC" ")" { ASC }
    | "SORT" "ASC" { ASC }
    | "SORT" "(" "DESC" ")" { DESC }
    | "SORT" "DESC" { DESC }
    | { ASC }

trim :: { Trim }
    : "NOTRIM" { TrimFalse }
    | { TrimTrue }

colrows :: { [ColRowData] }
    : colrow "," colrows { $1 : $3 }
    | colrow { [$1] }

colrow :: { ColRowData }
    : var "." rowOrCol "(" num ")" { ColRowData $1 $3 (read $5) }
    | var "." rowOrCol "(" "ALL" ")" { ColRowData $1 $3 0 }

rowOrCol :: { ColRow }
    : "COL" { COL }
    | "ROW" { ROW }

vars :: { [VarName] }
    : var "," vars { $1 : $3 }
    | var { [$1] }

literals :: { [Literal] }
    : literal "," literals { $1 : $3 }
    | literal { [$1] }

{
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at end of file"
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok


data Stmt = 
     Import FilePath VarName
    | Print VarName SortOrder Trim
    | Write VarName FilePath
    | Set VarName [Query]
    deriving (Show, Eq)

data Query = 
    Get [ColRowData]
    | Cross [VarName]
    | Filter FilterQuery
    | Merge MergeType VarName VarName Int
    | Concat ColRowData [Literal]
    | Union [VarName]
    | Distinct VarName
    | Limit Int
    deriving (Show, Eq)

data MergeType = LeftMerge | RightMerge deriving (Show, Eq)
data Trim = TrimTrue | TrimFalse deriving (Show, Eq)

data ColRow = COL | ROW deriving (Eq, Show)
data SortOrder = ASC | DESC deriving (Eq, Show)

data Operator = Equal | NotEqual | LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual deriving (Eq, Show)
data FilterQuery = FilterColRow ColRowData Operator ColRowData | FilterColRowOperand ColRowData Operator Operand | FilterColRowIsNull ColRowData | FilterColRowIsNotNull ColRowData deriving (Show, Eq) 

data Operand = OperandNum Int | OperandLiteral String | OperandFloat Float deriving (Eq, Show)
data ColRowData = 
    ColRowData VarName ColRow Int
    deriving (Show, Eq)

type CSVFilePath = String
type VarName = String
type Literal = String

}