module Main (main) where

import Engine
import Parser
import Types

-- Parsea la linea y si sale bien la corre sobre la base
executeLine :: String -> Database -> Either DbError (Database, Maybe String)
executeLine lineText database = parseSql lineText >>= (`runQuery` database)

-- Si hay mensaje, lo imprime
printOutput :: (Database, Maybe String) -> IO Database
printOutput databaseAndMessage =
  (maybe (pure ()) putStrLn . snd) databaseAndMessage
    >> pure (fst databaseAndMessage)

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
