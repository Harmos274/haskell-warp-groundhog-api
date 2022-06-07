{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module DB.Users
  ( Users (..),
    getAllUsers,
    insertUser,
  ) where

import Database.Groundhog.TH (mkPersist, defaultCodegenConfig, groundhog)
import Database.Groundhog (select, Cond (CondEmpty), insert, get)
import Database.Groundhog.Core (PersistBackend)
import Data.Maybe (fromJust)
import Data.Functor ((<&>))

data Users = Users { userName :: String, phone :: String } deriving Show

mkPersist defaultCodegenConfig [groundhog|
- entity: Users
  constructors:
    - name: Users
      fields:
        - name: userName
          dbName: name
      uniques:
        - name: NameConstraint
          fields: [userName]
|]

getAllUsers :: PersistBackend m => m [Users]
getAllUsers = select CondEmpty

insertUser :: PersistBackend m => Users -> m Users
insertUser user = insert user >>= get <&> fromJust
