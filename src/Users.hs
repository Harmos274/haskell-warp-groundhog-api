{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards, MultiParamTypeClasses, Strict #-}

module Users
  ( usersRouter,
  )
  where

import Network.Wai (Response, Request, responseBuilder, requestMethod, consumeRequestBodyStrict)
import Network.HTTP.Types.Status (ok200, notFound404, created201, badRequest400)
import Data.Binary.Builder (empty)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, decode)
import Data.Convertible.Base (Convertible (..), convert)
import Database.Groundhog.Postgresql (select, Cond (CondEmpty), Postgresql)
import Data.Functor ((<&>))
import Database.Groundhog.Core (runDbConn)

import DB.Users (Users (..))
import Core (jsonResponse, jsonErrorResponse, Cause (..))
import Database.Groundhog (insert, get)
import Data.Maybe (fromJust)

data User = User { name :: String, phone :: String } deriving (Generic, Show)

instance ToJSON User
instance FromJSON User

instance Convertible Users User where
  safeConvert Users { .. } = Right $ User userName phone

instance Convertible User Users where
  safeConvert User { .. } = Right $ Users name phone

usersRouter :: Postgresql -> Request -> IO Response
usersRouter db req | requestMethod req == "GET"  = getAllUsers db <&> jsonResponse ok200
usersRouter db req | requestMethod req == "POST" = consumeRequestBodyStrict req >>= (flip postUsers db . decode)
usersRouter _  _                                 = return $ responseBuilder notFound404 [] empty

getAllUsers :: Postgresql -> IO [User]
getAllUsers = runDbConn $ select CondEmpty <&> map (convert :: Users -> User)

postUsers :: Maybe User -> Postgresql -> IO Response
postUsers (Just user) db = postUsers' user db <&> jsonResponse created201
postUsers Nothing     _  = return $ jsonErrorResponse badRequest400 (Cause "Invalid body.")

postUsers' :: User -> Postgresql -> IO User
postUsers' user = runDbConn $ insert ((convert :: User -> Users) user) >>= get <&> ((convert :: Users -> User) . fromJust)
