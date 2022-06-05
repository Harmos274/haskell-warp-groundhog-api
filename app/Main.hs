module Main where

import DB.Users (Users (..), Tree (..))
import Database.Groundhog.Postgresql (withPostgresqlConn, runMigration, migrate, select, Cond (CondEmpty), insert, get)
import Lib (runAppOnPort)
import Control.Monad.IO.Class (liftIO)
import Database.Groundhog.Core (PersistBackend, runDbConn')

main :: IO ()
main = withPostgresqlConn pgConnectionString $ runDbConn' $ do
  runMigrations
  --insert (Users "Monsieur" "fdsfjdsfkjl") >>= get >>= liftIO . print
  --select CondEmpty >>= liftIO . (\users -> print (users :: [Users]))
  liftIO (putStrLn ("Listening on " ++ show port ++ ".") >> runAppOnPort port)

runMigrations :: PersistBackend m => m ()
--runMigrations =  mapM_ (createMigration >=> executeMigration)  [migrate (undefined :: Tree)]
runMigrations = runMigration $ foldl (>>) (return ())
  [
    migrate (undefined :: Users),
    migrate (undefined :: Tree)
  ]

pgConnectionString :: String
pgConnectionString = "postgresql://postgre:postgre@localhost:5432/db"

port :: Int
port = 3000