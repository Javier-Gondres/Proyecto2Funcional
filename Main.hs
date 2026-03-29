module Main (main) where

import Engine
import Parser
import Types

-- Parsea y ejecuta una linea SQL en la base de datos
executeLine :: String -> Database -> Either DbError (Database, Maybe String)
executeLine line db = parseSql line >>= (`runQuery` db)

-- Imprime el resultado de una ejecucion
printOutput :: (Database, Maybe String) -> IO Database
printOutput p = (maybe (pure ()) putStrLn . snd) p >> pure (fst p)

-- Maneja el resultado de una ejecucion, imprimiendo errores o salidas
handleResult :: Either DbError (Database, Maybe String) -> IO (Maybe Database)
handleResult = either (\e -> print e >> pure Nothing) (fmap Just . printOutput)

-- Main en bucle para ejecutar las consultas SQL
main :: IO ()
main = loop emptyDatabase
  where
    loop db = do
      putStr "SQL> "
      line <- getLine
      if line == ":q"
        then putStrLn "Hablamo ahorita compai"
        else do
          md <- handleResult (executeLine line db)
          case md of
            Nothing -> loop db
            Just db' -> loop db'
