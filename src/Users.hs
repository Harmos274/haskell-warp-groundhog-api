{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards, MultiParamTypeClasses, Strict #-}

module Users
  ( usersRouter,
  )
  where

import Network.Wai (Response, Request, responseBuilder, requestMethod)
import Network.HTTP.Types.Status (status200, status404, Status)
import Data.Binary.Builder (empty)
import Blaze.ByteString.Builder.ByteString (copyByteString)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encode)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import Data.Convertible.Base (Convertible (..), convert)

import DB.Users (Users (..))
import Database.Groundhog.Postgresql (select, Cond (CondEmpty), PersistBackend, Conn)
import Data.Functor ((<&>))

data User =  User {name :: String, phone :: String} deriving (Generic, Show)

instance ToJSON User
instance FromJSON User

instance Convertible Users User where
  safeConvert Users { .. } = Right $ User userName phone

usersRouter :: PersistBackend m => Request -> m Response
usersRouter req | requestMethod req == "GET"  = getUsers req
usersRouter req | requestMethod req == "POST" = getUsers req
usersRouter _                                 = return $ responseBuilder status404 [] empty

getUsers :: PersistBackend m => Request -> m Response
getUsers _ = (select CondEmpty <&> getUsers') <&> jsonResponse status200

getUsers' ::  [Users] -> ByteString
getUsers' = toStrict . encode . map (convert :: Users -> User)

jsonResponse :: Status -> ByteString -> Response
jsonResponse status = responseBuilder status [("Content-Type", "application/json")] . copyByteString

postUsers :: Request -> Response
postUsers request = responseBuilder status200 [("Content-Type", "application/json")] $ copyByteString "{\"hello\": \"world\"}"