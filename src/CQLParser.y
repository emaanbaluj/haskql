{
module CQLParser where
import CQLexer
import Debug.Trace (trace)
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
    "UPDATE" { PT _ TokenUPDATE }
    "SET" { PT _ TokenSET }
    "CROSS" { PT _ TokenCROSS }
    "NOTRIM" { PT _ TokenNOTRIM }
    "(" { PT _ TokenLPAREN }
    ")" { PT _ TokenRPAREN }
    "," { PT _ TokenCOMMA }
    ";" { PT _ TokenSEMICOLON }
    "==" { PT _ TokenEQ }
    "!=" { PT _ TokenNEQ }
    "<=" { PT _ TokenLE }
    ">=" { PT _ TokenGE }
    "<" { PT _ TokenLT }
    ">" { PT _ TokenGT }
    "+" { PT _ TokenPLUS }
    "-" { PT _ TokenMINUS }
    "/" { PT _ TokenDIVIDE }
    "*" { PT _ TokenMULTIPLY }
    "FROM" { PT _ TokenFROM }
    "." { PT _ TokenDOT }
    "FILTER" { PT _ TokenFILTER }
    "DISTINCT" { PT _ TokenDISTINCT }
    "GET" { PT _ TokenGET }
    "LIMIT" { PT _ TokenLIMIT }
    "REPLACE" { PT _ TokenREPLACE }
    "WITH" { PT _ TokenWITH }
    "UNION" { PT _ TokenUNION }
    "CONCAT" { PT _ TokenCONCAT }
    "ZIP" { PT _ TokenZIP }
    "STACK" { PT _ TokenSTACK }
    "WRITE" { PT _ TokenWRITE }
    "TO" { PT _ TokenTO }
    "COL" { PT _ TokenCOL }
    "ROW" { PT _ TokenROW }
    "PRINT" { PT _ TokenPRINT }
    "ASC" { PT _ TokenASC }
    "DESC" { PT _ TokenDESC }
    "LEFT_MERGE" { PT _ TokenLEFTMERGE }
    "RIGHT_MERGE" { PT _ TokenRIGHTMERGE }
    "ON" { PT _ TokenON }
    "IS" { PT _ TokenIS }
    "NOT" { PT _ TokenNOT }
    "NULL" { PT _ TokenNULL }
    "MAP" { PT _ TokenMAP }
    "UPPER" { PT _ TokenUPPER }
    "LOWER" { PT _ TokenLOWER }
    "IN"    { PT _ TokenIN }
    "TRANSPOSE" { PT _ TokenTRANSPOSE }
    "WHERE" { PT _ TokenWHERE }
    "NOSORT" { PT _ TokenNOSORT }
    "INSERT" { PT _ TokenINSERT }
    "REVERSE" { PT _ TokenREVERSE }
    "ARITY" { PT _ TokenARITY }
%%
stmts :: { [ Stmt ] }
    : stmt stmts { $1 : $2 }
    | stmt { [$1] }
    
stmt :: { Stmt }
    : "IMPORT" csvfilepath "AS" var ";" { Import $2 $4 }
    | "SET" var "AS" "(" queries ")" ";" { Set $2 $5 }
    | "SET" var "AS" queries ";" { Set $2 $4 }
    | "UPDATE" var "SET" "(" literals ")" "WHERE" filterquery ";" { Update $2 $5 $8 }
    | "WRITE" var "TO" csvfilepath ";" { Write $2 $4 }
    | "PRINT" colrow sort trim ";" { PrintColRow $2 $3 $4 }
    | "PRINT" var "." rowOrCol "(" num ")" "." rowOrCol "(" num ")" "TO" var ";" { Access2D $2 $4 (read $6) $9 (read $11) $14 }
    | "PRINT" var sort trim ";" { Print $2 $3 $4 }
    | "MAP" "(" expr ")" "IN" var "AS" var ";" { Map $3 $6 $8 }
    | "INSERT" "ROW" "(" literals ")" "IN" var "AS" var ";" { Insert ROW $4 $7 $9 }
    | "INSERT" "COL" "(" literals ")" "IN" var "AS" var ";" { Insert COL $4 $7 $9 }
    | "TRANSPOSE" var "AS" var ";" { Transpose $2 $4 }

queries :: { [Query] }
    : query "IN" queries { $1 : $3 }
    | query { [$1] }

query :: { Query }
    : "GET" colrows { Get $2 }
    | "CROSS" "(" vars ")" { Cross $3 }
    | "ZIP" "(" vars ")" { Zip $3 }
    | "STACK" "(" vars ")" { Stack $3 }
    | "FILTER" var "WHERE" filterquery { Filter $2 $4 }
    | "DISTINCT" var { Distinct $2 }
    | "UNION" "(" vars ")" { Union $3 }
    | "LIMIT" num "FROM" var { Limit (read $2) $4 }
    | "CONCAT" colrow "WITH" "(" literals ")" { Concat $2 $5 }
    | "REPLACE" colrow "WITH" colrow { Replace $2 $4 }
    | "REVERSE" "ROW" var { Reverse ROW $3 }
    | "REVERSE" "COL" var { Reverse COL $3 }
    | var "-" var { Complement $1 $3 }
    | mergetype "(" var "," var ")" "ON" "COL" num { Merge $1 $3 $5 (read $9) }

mergetype :: { MergeType }
    : "LEFT_MERGE" { LeftMerge }
    | "RIGHT_MERGE" { RightMerge }

filterquery :: { FilterQuery }
    : col operator col { FilterColRow $1 $2 $3 }
    | col operator operand { FilterColRowOperand $1 $2 $3 }
    | col "IS" "NULL" { FilterColRowIsNull $1 }
    | col "IS" "NOT" "NULL" { FilterColRowIsNotNull $1 }
    | col "IS" "NOT" "IN" colrow { FilterColRowIsNotIn $1 $5 }
    | col "IS" "IN" colrow { FilterColRowIsIn $1 $4 }

col :: { ColData }
    : "COL" "(" num ")" { ColData (read $3) }

operand :: { Operand }
    : num { OperandNum (read $1) }
    | float { OperandFloat (read $1) }
    | literal { OperandLiteral $1 }
    | "ARITY" var { OperandArity $2 }

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
    | "NOSORT"       { NO }
    | { ASC }

trim :: { Trim }
    : "NOTRIM" { TrimFalse }
    | { TrimTrue }

colrows :: { [ColRowData] }
    : colrow "," colrows { $1 : $3 }
    | colrow { [$1] }

colrow :: { ColRowData }
    : var "." rowOrCol "(" num ")" { ColRowData $1 $3 (read $5) }
    | "NULL" { ColRowData "NULL" COL 0 }

rowOrCol :: { ColRow }
    : "COL" { COL }
    | "ROW" { ROW }

vars :: { [VarName] }
    : var "," vars { $1 : $3 }
    | var { [$1] }

literals :: { [Literal] }
    : literal "," literals { $1 : $3 }
    | literal { [$1] }

expr :: { Expr }
    : "+" num { AddN (read $2) }
    | "-" num { SubN (read $2) }
    | "/" num { DivideN (read $2) }
    | "*" num { MultiplyN (read $2) }
    | "REPLACE" literal literal { ReplaceWith $2 $3 }
    | "UPPER" { ToUpper }
    | "LOWER" { ToLower }
    | "NOT"   { Not }

{
parseError :: [PosnToken] -> a
parseError [] = error "Parse error at end of file"
parseError (tok:_) = error $ "Parse error at " ++ tokenPosn tok

data Stmt = 
     Import FilePath VarName
    | Print VarName SortOrder Trim
    | Update VarName [Literal] FilterQuery
    | PrintColRow ColRowData SortOrder Trim
    | Write VarName FilePath
    | Transpose VarName VarName
    | Set VarName [Query]
    | Map Expr VarName VarName 
    | Insert ColRow [Literal] VarName VarName
    | Access2D VarName ColRow Int ColRow Int VarName
    deriving (Show, Eq)

data Query = 
    Get [ColRowData]
    | Cross [VarName]
    | Filter VarName FilterQuery
    | Zip [VarName]
    | Stack [VarName]
    | Merge MergeType VarName VarName Int
    | Concat ColRowData [Literal]
    | Union [VarName]
    | Distinct VarName
    | Reverse ColRow VarName
    | Limit Int VarName
    | Replace ColRowData ColRowData
    | Complement VarName VarName
    deriving (Show, Eq)

data Expr
  = AddN Int      
  | SubN Int      
  | ToUpper
  | ReplaceWith Literal Literal
  | ToLower
  | Not
  | DivideN Double
  | MultiplyN Double
  deriving (Eq, Show)

data MergeType = LeftMerge | RightMerge deriving (Show, Eq)
data Trim = TrimTrue | TrimFalse deriving (Show, Eq)

data ColRow = COL | ROW deriving (Eq, Show)
data SortOrder = ASC | DESC | NO deriving (Eq, Show)

data Operator = Equal | NotEqual | LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual deriving (Eq, Show)
data FilterQuery = FilterColRow ColData Operator ColData | FilterColRowOperand ColData Operator Operand | FilterColRowIsNull ColData | FilterColRowIsNotNull ColData | FilterColRowIsNotIn ColData ColRowData | FilterColRowIsIn ColData ColRowData deriving (Show, Eq) 

data Operand = OperandNum Int | OperandLiteral String | OperandFloat Float | OperandArity VarName deriving (Eq, Show)
data ColRowData = 
    ColRowData VarName ColRow Int
    deriving (Show, Eq)
data ColData = 
    ColData Int
    deriving (Show, Eq)
type CSVFilePath = String
type VarName = String
type Literal = String

}
