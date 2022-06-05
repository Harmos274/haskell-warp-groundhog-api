{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving #-}

module DB.Users
  ( Users (..),
    Tree (..),
  ) where

import Database.Groundhog.TH (mkPersist, defaultCodegenConfig, groundhog)
import Database.Groundhog.Postgresql ()

data Users = Users { userName :: String, phone :: String } deriving Show

data Tree = Tree { leaf :: String } deriving Show

mkPersist defaultCodegenConfig [groundhog|
- entity: Tree
  constructors:
    - name: Tree
      fields:
        - name: leaf
|]

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
