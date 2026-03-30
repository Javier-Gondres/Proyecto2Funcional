module Types where

-- Tipos de valores en la base de datos: entero, stringo nulo
data Value
  = VInt Int
  | VString String
  | VNull
  deriving (Eq)

instance Show Value where
  show (VInt n) = show n
  show (VString s) = show s
  show VNull = "NULL"

-- Alias para nombres de columnas y tablas
type ColumnName = String
type TableName = String
-- Una fila es una lista de valores
type Row = [Value]

-- Estructura de una tabla con columnas y filas
data Table = Table
  { tableColumns :: [ColumnName]
  , tableRows :: [Row]
  }

-- Base de datos como mapa de nombres a tablas
newtype Database = Database [(TableName, Table)]

-- Base de datos vacía
emptyDatabase :: Database
emptyDatabase = Database []

-- Consultas SQL soportadas
data SqlQuery
  = CreateTable TableName [ColumnName]
  | Insert TableName [Value]
  | Select (Maybe [ColumnName]) TableName (Maybe WhereExpr)
  | Delete TableName (Maybe WhereExpr)
  deriving (Eq, Show)

-- Expresiones WHERE para filtros
data WhereExpr
  = WCompare ColumnName CmpOp Value
  | WAnd WhereExpr WhereExpr
  | WOr WhereExpr WhereExpr
  deriving (Eq, Show)

-- Operadores de comparación
data CmpOp = CmpEq | CmpLt | CmpGt
  deriving (Eq, Show)

-- Errores posibles en la base de datos
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
