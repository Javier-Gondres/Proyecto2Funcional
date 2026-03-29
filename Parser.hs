module Parser (parseSql) where

import Data.Char (isAlphaNum, isDigit, isLetter, isSpace, toLower)

import Types

data Tok
  = TWord String
  | TInt Int
  | TString String
  | TComma
  | TLParen
  | TRParen
  | TStar
  | TEq
  | TLt
  | TGt
  deriving (Eq, Show)

-- Convierte cadena a minusculas
lower :: String -> String
lower = map toLower

-- Parsea una cadena SQL a una consulta
parseSql :: String -> Either DbError SqlQuery
parseSql s =
  case tokenize (trim s) of
    Left e -> Left e
    Right ts ->
      case parseStatement ts of
        Left e -> Left e
        Right (q, rest) ->
          if null rest then Right q else Left . SyntaxError $ "Tokens sobrantes: " ++ show rest

-- Elimina espacios en blanco al inicio y final
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace . reverse . dropWhile isSpace

tokenize :: String -> Either DbError [Tok]
tokenize [] = Right []
tokenize s =
  let s' = dropWhile isSpace s
   in if null s'
        then Right []
        else case s' of
          [] -> Right []
          ',' : rest -> (TComma :) <$> tokenize rest
          '(' : rest -> (TLParen :) <$> tokenize rest
          ')' : rest -> (TRParen :) <$> tokenize rest
          '*' : rest -> (TStar :) <$> tokenize rest
          '=' : rest -> (TEq :) <$> tokenize rest
          '<' : rest -> (TLt :) <$> tokenize rest
          '>' : rest -> (TGt :) <$> tokenize rest
          '"' : rest -> stringLit rest
          c : rest
            | isDigit c ->
                let (ds, rest') = span isDigit (c : rest)
                 in case reads ds of
                      [(n, "")] -> (TInt n :) <$> tokenize rest'
                      _ -> Left $ SyntaxError $ "Número inválido: " ++ ds
            | isLetter c || c == '_' ->
                let (w, rest') = span (\x -> isAlphaNum x || x == '_') (c : rest)
                 in (TWord w :) <$> tokenize rest'
            | otherwise ->
                Left . SyntaxError $ "Carácter inesperado: " ++ [c]
  where
    -- Parsea literales de cadena
    stringLit t =
      case break (== '"') t of
        (inside, '"' : rest) -> (TString inside :) <$> tokenize rest
        _ -> Left $ SyntaxError "Cadena sin cerrar con \""

-- Parsea una declaración SQL
parseStatement :: [Tok] -> Either DbError (SqlQuery, [Tok])
parseStatement [] = Left $ SyntaxError "Entrada vacía"
parseStatement (TWord w : ts)
  | lower w == "create" = parseCreate ts
  | lower w == "insert" = parseInsert ts
  | lower w == "delete" = parseDelete ts
  | lower w == "select" = parseSelect ts
parseStatement ts = Left . SyntaxError $ "Se esperaba CREATE, INSERT, DELETE o SELECT, obtuve: " ++ show (take 5 ts)

-- Parsea CREATE TABLE
parseCreate :: [Tok] -> Either DbError (SqlQuery, [Tok])
parseCreate (TWord w : ts)
  | lower w == "table" =
      case ts of
        (TWord tName : TLParen : ts') -> do
          (cols, rest) <- parseIdentsComma ts'
          case rest of
            TRParen : rest' -> Right (CreateTable tName cols, rest')
            _ -> Left $ SyntaxError "Falta ) en CREATE TABLE"
        _ -> Left $ SyntaxError "CREATE TABLE: se esperaba nombre e ("
parseCreate _ = Left $ SyntaxError "CREATE TABLE: falta TABLE"

-- Parsea lista de identificadores separados por coma
parseIdentsComma :: [Tok] -> Either DbError ([String], [Tok])
parseIdentsComma (TWord c : TComma : ts) = do
  (cs, rest) <- parseIdentsComma ts
  Right (c : cs, rest)
parseIdentsComma (TWord c : ts) = Right ([c], ts)
parseIdentsComma ts = Left $ SyntaxError $ "Lista de columnas inválida cerca de: " ++ show (take 8 ts)

-- Parsea INSERT
parseInsert :: [Tok] -> Either DbError (SqlQuery, [Tok])
parseInsert (TWord w : ts)
  | lower w == "into" =
      case ts of
        (TWord tName : TWord v : ts')
          | lower v == "values" ->
              case ts' of
                TLParen : ts'' -> do
                  (vals, rest) <- parseValues ts''
                  case rest of
                    TRParen : rest' -> Right (Insert tName vals, rest')
                    _ -> Left $ SyntaxError "Falta ) en INSERT"
                _ -> Left $ SyntaxError "INSERT: se esperaba ( después de VALUES"
        _ -> Left $ SyntaxError "INSERT: se esperaba INTO nombre VALUES"
parseInsert _ = Left $ SyntaxError "INSERT: falta INTO"

-- Parsea lista de valores
parseValues :: [Tok] -> Either DbError ([Value], [Tok])
parseValues ts = do
  (v, ts1) <- oneValue ts
  case ts1 of
    TComma : ts2 -> do
      (vs, rest) <- parseValues ts2
      Right (v : vs, rest)
    _ -> Right ([v], ts1)

-- Parsea un valor individual
oneValue :: [Tok] -> Either DbError (Value, [Tok])
oneValue (TInt n : ts) = Right (VInt n, ts)
oneValue (TString s : ts) = Right (VString s, ts)
oneValue (TWord w : ts)
  | lower w == "null" = Right (VNull, ts)
oneValue ts = Left $ SyntaxError $ "Valor inválido en VALUES: " ++ show (take 6 ts)

-- Parsea DELETE
parseDelete :: [Tok] -> Either DbError (SqlQuery, [Tok])
parseDelete (TWord w : ts)
  | lower w == "from" =
      case ts of
        (TWord tName : rest) -> do
          (mw, rest') <- parseOptWhere rest
          Right (Delete tName mw, rest')
        _ -> Left $ SyntaxError "DELETE: falta nombre de tabla"
parseDelete _ = Left $ SyntaxError "DELETE: falta FROM"

-- Parsea SELECT
parseSelect :: [Tok] -> Either DbError (SqlQuery, [Tok])
parseSelect ts0 = do
  (mcols, ts1) <- parseCols ts0
  case ts1 of
    (TWord w : TWord tName : rest)
      | lower w == "from" -> do
          (mw, rest') <- parseOptWhere rest
          Right (Select mcols tName mw, rest')
    _ -> Left $ SyntaxError "SELECT: se esperaba FROM tabla"

-- Parsea columnas en SELECT
parseCols :: [Tok] -> Either DbError (Maybe [String], [Tok])
parseCols (TStar : ts) = Right (Nothing, ts)
parseCols ts = do
  (ids, rest) <- parseIdentsComma ts
  Right (Just ids, rest)

-- Parsea WHERE opcional
parseOptWhere :: [Tok] -> Either DbError (Maybe WhereExpr, [Tok])
parseOptWhere [] = Right (Nothing, [])
parseOptWhere (TWord w : ts)
  | lower w == "where" = do
      (e, rest) <- parseExpr ts
      Right (Just e, rest)
parseOptWhere ts = Right (Nothing, ts)

-- Parsea expresión WHERE
parseExpr :: [Tok] -> Either DbError (WhereExpr, [Tok])
parseExpr = parseOr

-- Parsea OR
parseOr :: [Tok] -> Either DbError (WhereExpr, [Tok])
parseOr ts = do
  (e1, ts1) <- parseAnd ts
  case ts1 of
    (TWord w : rest)
      | lower w == "or" -> do
          (e2, ts2) <- parseOr rest
          Right (WOr e1 e2, ts2)
    _ -> Right (e1, ts1)

-- Parsea AND
parseAnd :: [Tok] -> Either DbError (WhereExpr, [Tok])
parseAnd ts = do
  (e1, ts1) <- parseAtom ts
  case ts1 of
    (TWord w : rest)
      | lower w == "and" -> do
          (e2, ts2) <- parseAnd rest
          Right (WAnd e1 e2, ts2)
    _ -> Right (e1, ts1)

-- Parsea comparacion o parentesis
parseAtom :: [Tok] -> Either DbError (WhereExpr, [Tok])
parseAtom (TLParen : ts) = do
  (e, ts1) <- parseExpr ts
  case ts1 of
    TRParen : rest -> Right (e, rest)
    _ -> Left $ SyntaxError "Falta ) en expresión WHERE"
parseAtom (TWord col : ts) = do
  (op, rest) <-
    case ts of
      TEq : r -> Right (CmpEq, r)
      TLt : r -> Right (CmpLt, r)
      TGt : r -> Right (CmpGt, r)
      _ -> Left $ SyntaxError "Se esperaba =, < o > después de columna"
  (v, rest') <- parseLiteral rest
  Right (WCompare col op v, rest')
parseAtom ts = Left $ SyntaxError $ "WHERE: átomo inválido: " ++ show (take 6 ts)

-- Parsea comparación
parseLiteral :: [Tok] -> Either DbError (Value, [Tok])
parseLiteral (TInt n : rest) = Right (VInt n, rest)
parseLiteral (TString s : rest) = Right (VString s, rest)
parseLiteral (TWord w : rest)
  | lower w == "null" = Right (VNull, rest)
parseLiteral _ = Left $ SyntaxError "Literal inválido en comparación"
