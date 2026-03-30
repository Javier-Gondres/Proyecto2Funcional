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

toLowerString :: String -> String
toLowerString = map toLower

-- Lee el texto, lo parte en tokens y arma el comando. Si sobra basura al final, tira un error.
parseSql :: String -> Either DbError SqlQuery
parseSql inputText =
  case tokenize (trimWhitespace inputText) of
    Left dbError -> Left dbError
    Right tokens ->
      case parseStatement tokens of
        Left dbError -> Left dbError
        Right (parsedQuery, remainingTokens) ->
          if null remainingTokens
            then Right parsedQuery
            else Left . SyntaxError $ "Tokens sobrantes: " ++ show remainingTokens

-- Elimina espacios en blanco al inicio y final
trimWhitespace :: String -> String
trimWhitespace = trimOnce . trimOnce
  where
    trimOnce = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Convierte el string en una lista de tokens (numeros, palabras, simbolos, comillas)
tokenize :: String -> Either DbError [Tok]
tokenize [] = Right []
tokenize inputText =
  let trimmedInput = dropWhile isSpace inputText
   in if null trimmedInput
        then Right []
        else case trimmedInput of
          [] -> Right []
          ',' : restOfInput -> (TComma :) <$> tokenize restOfInput
          '(' : restOfInput -> (TLParen :) <$> tokenize restOfInput
          ')' : restOfInput -> (TRParen :) <$> tokenize restOfInput
          '*' : restOfInput -> (TStar :) <$> tokenize restOfInput
          '=' : restOfInput -> (TEq :) <$> tokenize restOfInput
          '<' : restOfInput -> (TLt :) <$> tokenize restOfInput
          '>' : restOfInput -> (TGt :) <$> tokenize restOfInput
          '"' : restOfInput -> parseStringLiteral restOfInput
          character : restOfInput
            | isDigit character ->
                let (digitRun, afterDigits) = span isDigit (character : restOfInput)
                 in case reads digitRun of
                      [(parsedInt, "")] -> (TInt parsedInt :) <$> tokenize afterDigits
                      _ -> Left $ SyntaxError $ "Número inválido: " ++ digitRun
            | isLetter character || character == '_' ->
                let (identifier, afterIdentifier) =
                      span (\ch -> isAlphaNum ch || ch == '_') (character : restOfInput)
                 in (TWord identifier :) <$> tokenize afterIdentifier
            | otherwise ->
                Left . SyntaxError $ "Carácter inesperado: " ++ [character]
  where
    parseStringLiteral textAfterQuote =
      case break (== '"') textAfterQuote of
        (stringContents, '"' : restAfterClosingQuote) ->
          (TString stringContents :) <$> tokenize restAfterClosingQuote
        _ -> Left $ SyntaxError "Cadena sin cerrar con \""

-- Primera palabra dice si es create/insert/delete/select. Devuelve lo que sobra.
parseStatement :: [Tok] -> Either DbError (SqlQuery, [Tok])
parseStatement [] = Left $ SyntaxError "Entrada vacía"
parseStatement (TWord firstWord : remainingTokens)
  | toLowerString firstWord == "create" = parseCreate remainingTokens
  | toLowerString firstWord == "insert" = parseInsert remainingTokens
  | toLowerString firstWord == "delete" = parseDelete remainingTokens
  | toLowerString firstWord == "select" = parseSelect remainingTokens
parseStatement tokens =
  Left . SyntaxError $
    "Se esperaba CREATE, INSERT, DELETE o SELECT, obtuve: " ++ show (take 5 tokens)

-- Parsea CREATE TABLE
parseCreate :: [Tok] -> Either DbError (SqlQuery, [Tok])
parseCreate (TWord keyword : remainingTokens)
  | toLowerString keyword == "table" =
      case remainingTokens of
        (TWord tableName : TLParen : tokensInsideParens) -> do
          (columnNames, afterColumns) <- parseIdentsComma tokensInsideParens
          case afterColumns of
            TRParen : afterClosingParen ->
              Right (CreateTable tableName columnNames, afterClosingParen)
            _ -> Left $ SyntaxError "Falta ) en CREATE TABLE"
        _ -> Left $ SyntaxError "CREATE TABLE: se esperaba nombre e ("
parseCreate _ = Left $ SyntaxError "CREATE TABLE: falta TABLE"

-- Parsea lista de identificadores separados por coma
parseIdentsComma :: [Tok] -> Either DbError ([String], [Tok])
parseIdentsComma (TWord columnName : TComma : remainingTokens) = do
  (restColumnNames, afterList) <- parseIdentsComma remainingTokens
  Right (columnName : restColumnNames, afterList)
parseIdentsComma (TWord columnName : remainingTokens) = Right ([columnName], remainingTokens)
parseIdentsComma tokens =
  Left . SyntaxError $
    "Lista de columnas inválida cerca de: " ++ show (take 8 tokens)

-- Parsea INSERT
parseInsert :: [Tok] -> Either DbError (SqlQuery, [Tok])
parseInsert (TWord keyword : remainingTokens)
  | toLowerString keyword == "into" =
      case remainingTokens of
        (TWord tableName : TWord nextWord : afterTable)
          | toLowerString nextWord == "values" ->
              case afterTable of
                TLParen : tokensInsideParens -> do
                  (insertedValues, afterValues) <- parseValues tokensInsideParens
                  case afterValues of
                    TRParen : afterClosingParen ->
                      Right (Insert tableName insertedValues, afterClosingParen)
                    _ -> Left $ SyntaxError "Falta ) en INSERT"
                _ -> Left $ SyntaxError "INSERT: se esperaba ( después de VALUES"
        _ -> Left $ SyntaxError "INSERT: se esperaba INTO nombre VALUES"
parseInsert _ = Left $ SyntaxError "INSERT: falta INTO"

-- Parsea lista de valores
parseValues :: [Tok] -> Either DbError ([Value], [Tok])
parseValues tokens = do
  (firstValue, tokensAfterFirst) <- oneValue tokens
  case tokensAfterFirst of
    TComma : tokensAfterComma -> do
      (restValues, finalTokens) <- parseValues tokensAfterComma
      Right (firstValue : restValues, finalTokens)
    _ -> Right ([firstValue], tokensAfterFirst)

-- Parsea un valor individual
oneValue :: [Tok] -> Either DbError (Value, [Tok])
oneValue (TInt intValue : remainingTokens) = Right (VInt intValue, remainingTokens)
oneValue (TString stringValue : remainingTokens) = Right (VString stringValue, remainingTokens)
oneValue (TWord word : remainingTokens)
  | toLowerString word == "null" = Right (VNull, remainingTokens)
oneValue tokens =
  Left . SyntaxError $ "Valor inválido en VALUES: " ++ show (take 6 tokens)

-- Parsea DELETE
parseDelete :: [Tok] -> Either DbError (SqlQuery, [Tok])
parseDelete (TWord keyword : remainingTokens)
  | toLowerString keyword == "from" =
      case remainingTokens of
        (TWord tableName : afterTableName) -> do
          (maybeWhereExpression, afterWhere) <- parseOptWhere afterTableName
          Right (Delete tableName maybeWhereExpression, afterWhere)
        _ -> Left $ SyntaxError "DELETE: falta nombre de tabla"
parseDelete _ = Left $ SyntaxError "DELETE: falta FROM"

-- Parsea SELECT
parseSelect :: [Tok] -> Either DbError (SqlQuery, [Tok])
parseSelect tokensAfterSelect = do
  (maybeSelectedColumns, tokensAfterColumns) <- parseCols tokensAfterSelect
  case tokensAfterColumns of
    (TWord fromKeyword : TWord tableName : afterFrom)
      | toLowerString fromKeyword == "from" -> do
          (maybeWhereExpression, afterWhere) <- parseOptWhere afterFrom
          Right
            ( Select maybeSelectedColumns tableName maybeWhereExpression
            , afterWhere
            )
    _ -> Left $ SyntaxError "SELECT: se esperaba FROM tabla"

-- Parsea columnas en SELECT
parseCols :: [Tok] -> Either DbError (Maybe [String], [Tok])
parseCols (TStar : remainingTokens) = Right (Nothing, remainingTokens)
parseCols tokens = do
  (identifiers, afterIdentifiers) <- parseIdentsComma tokens
  Right (Just identifiers, afterIdentifiers)

-- Parsea WHERE opcional
parseOptWhere :: [Tok] -> Either DbError (Maybe WhereExpr, [Tok])
parseOptWhere [] = Right (Nothing, [])
parseOptWhere (TWord keyword : remainingTokens)
  | toLowerString keyword == "where" = do
      (whereExpression, afterExpression) <- parseExpr remainingTokens
      Right (Just whereExpression, afterExpression)
parseOptWhere remainingTokens = Right (Nothing, remainingTokens)

-- En el WHERE, AND se resuelve primero y OR despues.
parseExpr :: [Tok] -> Either DbError (WhereExpr, [Tok])
parseExpr = parseOr

-- Lee una condicion; si aparece OR, une con otra condicion.
parseOr :: [Tok] -> Either DbError (WhereExpr, [Tok])
parseOr tokens = do
  (leftExpression, tokensAfterLeft) <- parseAnd tokens
  case tokensAfterLeft of
    (TWord orKeyword : restAfterOr)
      | toLowerString orKeyword == "or" -> do
          (rightExpression, tokensAfterRight) <- parseOr restAfterOr
          Right (WOr leftExpression rightExpression, tokensAfterRight)
    _ -> Right (leftExpression, tokensAfterLeft)

-- Lee una condicion; si aparece AND, une con otra condicion.
parseAnd :: [Tok] -> Either DbError (WhereExpr, [Tok])
parseAnd tokens = do
  (leftExpression, tokensAfterLeft) <- parseAtom tokens
  case tokensAfterLeft of
    (TWord andKeyword : restAfterAnd)
      | toLowerString andKeyword == "and" -> do
          (rightExpression, tokensAfterRight) <- parseAnd restAfterAnd
          Right (WAnd leftExpression rightExpression, tokensAfterRight)
    _ -> Right (leftExpression, tokensAfterLeft)

-- Lee una expresion o una comparacion y devuelve esa parte junto con los tokens que faltan por procesar.
parseAtom :: [Tok] -> Either DbError (WhereExpr, [Tok])
parseAtom (TLParen : remainingTokens) = do
  (innerExpression, tokensAfterInner) <- parseExpr remainingTokens
  case tokensAfterInner of
    TRParen : afterClosingParen -> Right (innerExpression, afterClosingParen)
    _ -> Left $ SyntaxError "Falta ) en expresión WHERE"
parseAtom (TWord columnName : remainingTokens) = do
  (comparisonOperator, tokensAfterOperator) <-
    case remainingTokens of
      TEq : rest -> Right (CmpEq, rest)
      TLt : rest -> Right (CmpLt, rest)
      TGt : rest -> Right (CmpGt, rest)
      _ -> Left $ SyntaxError "Se esperaba =, < o > después de columna"
  (literalValue, tokensAfterLiteral) <- parseLiteral tokensAfterOperator
  Right (WCompare columnName comparisonOperator literalValue, tokensAfterLiteral)
parseAtom tokens =
  Left . SyntaxError $ "WHERE: átomo inválido: " ++ show (take 6 tokens)

-- Convierte un token a un valor para comparaciones del WHERE.
parseLiteral :: [Tok] -> Either DbError (Value, [Tok])
parseLiteral (TInt intValue : remainingTokens) = Right (VInt intValue, remainingTokens)
parseLiteral (TString stringValue : remainingTokens) =
  Right (VString stringValue, remainingTokens)
parseLiteral (TWord word : remainingTokens)
  | toLowerString word == "null" = Right (VNull, remainingTokens)
parseLiteral _ = Left $ SyntaxError "Literal inválido en comparación"
