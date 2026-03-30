module Main (main) where

import Engine
import Parser
import Types

-- Parsea y ejecuta una linea SQL en la base de datos
executeLine :: String -> Database -> Either DbError (Database, Maybe String)
executeLine lineText database = parseSql lineText >>= (`runQuery` database)

-- Imprime el resultado de una ejecucion
printOutput :: (Database, Maybe String) -> IO Database
printOutput databaseAndMessage =
  (maybe (pure ()) putStrLn . snd) databaseAndMessage
    >> pure (fst databaseAndMessage)

-- Maneja el resultado de una ejecucion, imprimiendo errores o salidas
handleResult :: Either DbError (Database, Maybe String) -> IO (Maybe Database)
handleResult =
  either
    (\dbError -> print dbError >> pure Nothing)
    (fmap Just . printOutput)

{-
  Ejemplo de comandos para probar:

  CREATE TABLE personas (nombre, edad)
  INSERT INTO personas VALUES ("Javier", 20)
  INSERT INTO personas VALUES ("Yodariz", 18)
  INSERT INTO personas VALUES ("Fernando", 22)
  SELECT * FROM personas
  SELECT nombre FROM personas WHERE edad > 19
  DELETE FROM personas WHERE nombre = "Yodariz"
  SELECT * FROM personas
  :q
-}

main :: IO ()
main = loop emptyDatabase
  where
    loop database = do
      putStr "SQL> "
      lineText <- getLine
      if lineText == ":q"
        then putStrLn "Hablamo ahorita compai"
        else do
          maybeUpdatedDatabase <- handleResult (executeLine lineText database)
          case maybeUpdatedDatabase of
            Nothing -> loop database
            Just updatedDatabase -> loop updatedDatabase
