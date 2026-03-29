module Engine where

import Data.List (elemIndex, intercalate)

import Types 

lookupTable :: TableName -> Database -> Maybe Table
lookupTable name (Database m) = lookup name m

insertTable :: TableName -> Table -> Database -> Database
insertTable name tbl (Database m) = Database ((name, tbl) : filter ((/= name) . fst) m)

rowValue :: Table -> ColumnName -> Row -> Maybe Value
rowValue tbl col vals = do
  i <- elemIndex col (tableColumns tbl)
  if i < length vals then Just (vals !! i) else Nothing

runQuery :: SqlQuery -> Database -> Either DbError (Database, Maybe String)
runQuery q db = case q of
  CreateTable name cols -> runCreate name cols db
  Insert name vals -> runInsert name vals db
  Select mcols name mwhere -> runSelect mcols name mwhere db
  Delete name mwhere -> runDelete name mwhere db

runCreate :: TableName -> [ColumnName] -> Database -> Either DbError (Database, Maybe String)
runCreate name cols db =
  case lookupTable name db of
    Just _ -> Left $ DuplicateTable name
    Nothing ->
      let tbl = Table {tableColumns = cols, tableRows = []}
          db' = insertTable name tbl db
       in Right (db', Just $ "Tabla '" ++ name ++ "' creada.")

runInsert :: TableName -> [Value] -> Database -> Either DbError (Database, Maybe String)
runInsert name vals db =
  case lookupTable name db of
    Nothing -> Left $ UnknownTable name
    Just tbl ->
      let n = length (tableColumns tbl)
          m' = length vals
       in if n /= m'
            then Left $ ColumnCountMismatch n m'
            else
              let tbl' = tbl {tableRows = tableRows tbl ++ [vals]}
                  db' = insertTable name tbl' db
               in Right (db', Just $ "1 fila insertada en '" ++ name ++ "'.")

runSelect :: Maybe [ColumnName] -> TableName -> Maybe WhereExpr -> Database -> Either DbError (Database, Maybe String)
runSelect mcols name mwhere db =
  case lookupTable name db of
    Nothing -> Left $ UnknownTable name
    Just tbl ->
      let cols = maybe (tableColumns tbl) id mcols
          errCols = maybe [] (\cs -> filter (`notElem` tableColumns tbl) cs) mcols
       in case errCols of
            c : _ -> Left $ UnknownColumn c
            [] ->
              let rs = filter (matchWhere mwhere tbl) (tableRows tbl)
               in Right (db, Just $ formatSelectResult tbl cols rs)

runDelete :: TableName -> Maybe WhereExpr -> Database -> Either DbError (Database, Maybe String)
runDelete name mwhere db =
  case lookupTable name db of
    Nothing -> Left $ UnknownTable name
    Just tbl ->
      let kept = filter (not . matchWhere mwhere tbl) (tableRows tbl)
          n = length (tableRows tbl) - length kept
          tbl' = tbl {tableRows = kept}
          db' = insertTable name tbl' db
       in Right (db', Just $ show n ++ " fila(s) eliminada(s) de '" ++ name ++ "'.")

matchWhere :: Maybe WhereExpr -> Table -> Row -> Bool
matchWhere Nothing _ _ = True
matchWhere (Just e) tbl vals = evalWhere tbl e vals

evalWhere :: Table -> WhereExpr -> Row -> Bool
evalWhere tbl (WCompare col op lit) vals =
  case rowValue tbl col vals of
    Nothing -> False
    Just v -> cmp op v lit
evalWhere tbl (WAnd a b) vals = evalWhere tbl a vals && evalWhere tbl b vals
evalWhere tbl (WOr a b) vals = evalWhere tbl a vals || evalWhere tbl b vals

cmp :: CmpOp -> Value -> Value -> Bool
cmp CmpEq a b = a == b
cmp CmpLt a b = compareVal a b == LT
cmp CmpGt a b = compareVal a b == GT

compareVal :: Value -> Value -> Ordering
compareVal (VInt x) (VInt y) = compare x y
compareVal (VString x) (VString y) = compare x y
compareVal VNull _ = LT
compareVal _ VNull = GT
compareVal (VInt _) (VString _) = LT
compareVal (VString _) (VInt _) = GT

formatSelectResult :: Table -> [ColumnName] -> [Row] -> String
formatSelectResult tbl cols rs =
  let header = intercalate " | " cols
      line r = intercalate " | " $ map (showCell r) cols
      showCell r c = maybe "NULL" show (rowValue tbl c r)
      body = foldr (\r acc -> line r : acc) [] rs
   in if null rs then header ++ "\n(sin filas)" else unlines $ header : body
