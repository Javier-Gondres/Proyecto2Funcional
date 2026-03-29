module Types where

data Value
  = VInt Int
  | VString String
  | VNull
  deriving (Eq)

instance Show Value where
  show (VInt n) = show n
  show (VString s) = show s
  show VNull = "NULL"

type ColumnName = String
type TableName = String
type Row = [Value]

data Table = Table
  { tableColumns :: [ColumnName]
  , tableRows :: [Row]
  }

newtype Database = Database [(TableName, Table)]

emptyDatabase :: Database
emptyDatabase = Database []

data SqlQuery
  = CreateTable TableName [ColumnName]
  | Insert TableName [Value]
  | Select (Maybe [ColumnName]) TableName (Maybe WhereExpr)
  | Delete TableName (Maybe WhereExpr)
  deriving (Eq, Show)

data WhereExpr
  = WCompare ColumnName CmpOp Value
  | WAnd WhereExpr WhereExpr
  | WOr WhereExpr WhereExpr
  deriving (Eq, Show)

data CmpOp = CmpEq | CmpLt | CmpGt
  deriving (Eq, Show)

data DbError
  = UnknownTable TableName
  | UnknownColumn ColumnName
  | DuplicateTable TableName
  | ColumnCountMismatch Int Int
  | SyntaxError String
  | TypeError String
  deriving (Eq)

instance Show DbError where
  show (UnknownTable t) = "Tabla desconocida: " ++ t
  show (UnknownColumn c) = "Columna desconocida: " ++ c
  show (DuplicateTable t) = "La tabla ya existe: " ++ t
  show (ColumnCountMismatch expected got) =
    "Número de columnas incorrecto: se esperaban " ++ show expected ++ ", hay " ++ show got
  show (SyntaxError msg) = "Error de sintaxis: " ++ msg
  show (TypeError msg) = "Error de tipos: " ++ msg
