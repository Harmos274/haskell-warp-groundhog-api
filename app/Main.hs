module Main where

import DB.Users (Users (..))
import Database.Groundhog.Postgresql (Postgresql, withPostgresqlConn, runMigration, migrate)
import Lib (runAppOnPort)
import Database.Groundhog.Core (PersistBackend, runDbConn)

main :: IO ()
main = withPostgresqlConn pgConnectionString start

start :: Postgresql -> IO ()
start db = do 
  runDbConn runMigrations db
  putStrLn ("Listening on " ++ show port ++ ".") 
  runAppOnPort port db

runMigrations :: PersistBackend m => m ()
--runMigrations =  mapM_ (createMigration >=> executeMigration)  [migrate (undefined :: Tree)]
runMigrations = runMigration $ foldl (>>) (return ())
  [
    migrate (undefined :: Users)
  ]

pgConnectionString :: String
pgConnectionString = "postgresql://postgre:postgre@localhost:5432/db"

port :: Int
port = 3000