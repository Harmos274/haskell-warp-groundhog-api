{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards, MultiParamTypeClasses, Strict #-}

module Users
  ( usersRouter,
  )
  where

import GHC.Generics (Generic)
import Data.Functor ((<&>))
import Data.Aeson (ToJSON, FromJSON, decode)
import Data.Convertible.Base (Convertible (..), convert)
import Database.Groundhog.Postgresql (Postgresql)
import Database.Groundhog.Core (runDbConn)
import Network.Wai (Response, Request, requestMethod, consumeRequestBodyStrict)
import Network.HTTP.Types.Status (ok200, created201, badRequest400)

import Core (jsonResponse, jsonErrorResponse, json404NotFound, Cause (..))
import DB.Users (Users (..), getAllUsers, insertUser)

data User = User { name :: String, phone :: String } deriving (Generic, Show)

instance ToJSON User
instance FromJSON User

instance Convertible Users User where
  safeConvert Users { .. } = Right $ User userName phone

instance Convertible User Users where
  safeConvert User { .. } = Right $ Users name phone

usersRouter :: Postgresql -> Request -> IO Response
usersRouter db req | requestMethod req == "GET"  = runDbConn getAllUsers db <&> map (convert :: Users -> User) <&> jsonResponse ok200
usersRouter db req | requestMethod req == "POST" = consumeRequestBodyStrict req >>= (postUsers db . decode)
usersRouter _  _                                 = return json404NotFound

postUsers :: Postgresql -> Maybe User -> IO Response
postUsers  db (Just user) = runDbConn (insertUser $ convert user) db <&> (convert :: Users -> User) <&> jsonResponse created201
postUsers  _  Nothing     = return $ jsonErrorResponse badRequest400 (Cause "Invalid body.")
