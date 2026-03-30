module Engine where

import Data.List (elemIndex, intercalate)

import Types

lookupTable :: TableName -> Database -> Maybe Table
lookupTable tableName (Database tablesList) = lookup tableName tablesList

-- Guarda la tabla con ese nombre. Si ya habia una, la reemplaza
insertTable :: TableName -> Table -> Database -> Database
insertTable tableName table (Database tablesList) =
  Database ((tableName, table) : filter ((/= tableName) . fst) tablesList)

-- Sabe que numero de columna es y lee ese valor en la fila (mismo orden que el esquema)
rowValue :: Table -> ColumnName -> Row -> Maybe Value
rowValue table columnName rowValues = do
  columnIndex <- elemIndex columnName (tableColumns table)
  if columnIndex < length rowValues
    then Just (rowValues !! columnIndex)
    else Nothing

-- Mira que tipo de comando es y lo manda a la funcion que toca
runQuery :: SqlQuery -> Database -> Either DbError (Database, Maybe String)
runQuery sqlQuery database = case sqlQuery of
  CreateTable name columnNames -> runCreate name columnNames database
  Insert name rowValues -> runInsert name rowValues database
  Select maybeSelectedColumns name maybeWhereClause ->
    runSelect maybeSelectedColumns name maybeWhereClause database
  Delete name maybeWhereClause -> runDelete name maybeWhereClause database

runCreate :: TableName -> [ColumnName] -> Database -> Either DbError (Database, Maybe String)
runCreate tableName columnNames database =
  case lookupTable tableName database of
    Just _ -> Left $ DuplicateTable tableName
    Nothing ->
      let newTable = Table {tableColumns = columnNames, tableRows = []}
          updatedDatabase = insertTable tableName newTable database
       in Right (updatedDatabase, Just $ "Tabla '" ++ tableName ++ "' creada.")

runInsert :: TableName -> [Value] -> Database -> Either DbError (Database, Maybe String)
runInsert tableName rowValues database =
  case lookupTable tableName database of
    Nothing -> Left $ UnknownTable tableName
    Just table ->
      let expectedColumnCount = length (tableColumns table)
          providedValueCount = length rowValues
       in if expectedColumnCount /= providedValueCount
            then Left $ ColumnCountMismatch expectedColumnCount providedValueCount
            else
              let updatedTable = table {tableRows = tableRows table ++ [rowValues]}
                  updatedDatabase = insertTable tableName updatedTable database
               in Right (updatedDatabase, Just $ "1 fila insertada en '" ++ tableName ++ "'.")

-- Select: la base no cambia. Solo escoge filas y arma el texto a mostrar
runSelect ::
  Maybe [ColumnName] ->
  TableName ->
  Maybe WhereExpr ->
  Database ->
  Either DbError (Database, Maybe String)
runSelect maybeSelectedColumns tableName maybeWhereClause database =
  case lookupTable tableName database of
    Nothing -> Left $ UnknownTable tableName
    Just table ->
      let outputColumns = maybe (tableColumns table) id maybeSelectedColumns
          invalidColumns =
            maybe
              []
              (\columns -> filter (`notElem` tableColumns table) columns)
              maybeSelectedColumns
       in case invalidColumns of
            unknownColumn : _ -> Left $ UnknownColumn unknownColumn
            [] ->
              let matchingRows =
                    filter (matchWhere maybeWhereClause table) (tableRows table)
               in Right (database, Just $ formatSelectResult table outputColumns matchingRows)

-- Comando delete, se quedan las filas que no cumplen el where. Sin where se borra todo
runDelete :: TableName -> Maybe WhereExpr -> Database -> Either DbError (Database, Maybe String)
runDelete tableName maybeWhereClause database =
  case lookupTable tableName database of
    Nothing -> Left $ UnknownTable tableName
    Just table ->
      let rowsToKeep = filter (not . matchWhere maybeWhereClause table) (tableRows table)
          deletedRowCount = length (tableRows table) - length rowsToKeep
          updatedTable = table {tableRows = rowsToKeep}
          updatedDatabase = insertTable tableName updatedTable database
       in Right
            ( updatedDatabase
            , Just $
                show deletedRowCount
                  ++ " fila eliminada de '"
                  ++ tableName
                  ++ "'."
            )

-- Sin where, cualquier fila pasa. Con where, mira si cumple la condicion
matchWhere :: Maybe WhereExpr -> Table -> Row -> Bool
matchWhere Nothing _ _ = True
matchWhere (Just whereExpression) table rowValues =
  evalWhere table whereExpression rowValues

-- Where: comparar columna con valor, "and" de dos partes, "or" de dos partes
evalWhere :: Table -> WhereExpr -> Row -> Bool
evalWhere table (WCompare columnName comparisonOperator literalValue) rowValues =
  case rowValue table columnName rowValues of
    Nothing -> False
    Just cellValue ->
      applyComparison comparisonOperator cellValue literalValue
evalWhere table (WAnd leftSubexpression rightSubexpression) rowValues =
  evalWhere table leftSubexpression rowValues
    && evalWhere table rightSubexpression rowValues
evalWhere table (WOr leftSubexpression rightSubexpression) rowValues =
  evalWhere table leftSubexpression rowValues
    || evalWhere table rightSubexpression rowValues

-- Esta funcion toma el operador del WHERE (=, <, >) y lo aplica entre el valor de la celda y el valor literal 
applyComparison :: CmpOp -> Value -> Value -> Bool
applyComparison CmpEq leftValue rightValue = leftValue == rightValue
applyComparison CmpLt leftValue rightValue = compareValues leftValue rightValue == LT
applyComparison CmpGt leftValue rightValue = compareValues leftValue rightValue == GT

compareValues :: Value -> Value -> Ordering
compareValues (VInt leftInt) (VInt rightInt) = compare leftInt rightInt
compareValues (VString leftString) (VString rightString) = compare leftString rightString
compareValues VNull _ = LT
compareValues _ VNull = GT
compareValues (VInt _) (VString _) = LT
compareValues (VString _) (VInt _) = GT

-- Texto del select: titulos y luego cada fila
formatSelectResult :: Table -> [ColumnName] -> [Row] -> String
formatSelectResult table columnNames rows =
  let headerLine = intercalate " | " columnNames
      formatOneRow rowValues =
        intercalate " | " $ map (showOneCell rowValues) columnNames
      showOneCell rowValues columnName =
        maybe "NULL" show (rowValue table columnName rowValues)
      bodyLines =
        foldr
          (\rowValues accumulatedLines -> formatOneRow rowValues : accumulatedLines)
          []
          rows
   in if null rows
        then headerLine ++ "\n(sin filas)"
        else unlines $ headerLine : bodyLines
