module Main (main) where

import Engine
import Parser
import Types

-- | Parseo + ejecución pura (sin IO).
executeLine :: String -> Database -> Either DbError (Database, Maybe String)
executeLine line db = parseSql line >>= (`runQuery` db)

-- | Imprime el mensaje opcional de una ejecución exitosa (usa (.) sobre snd).
printOutput :: (Database, Maybe String) -> IO Database
printOutput p = (maybe (pure ()) putStrLn . snd) p >> pure (fst p)

handleResult :: Either DbError (Database, Maybe String) -> IO (Maybe Database)
handleResult = either (\e -> print e >> pure Nothing) (fmap Just . printOutput)

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
