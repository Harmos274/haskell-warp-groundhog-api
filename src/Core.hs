{-# LANGUAGE OverloadedStrings, Strict, DeriveGeneric, NamedFieldPuns #-}

module Core
  ( jsonResponse,
    jsonErrorResponse,
    Cause (..),
    APIError (..),
  ) where

import Network.Wai (Response, responseLBS)
import Network.HTTP.Types (Status (..))
import GHC.Generics (Generic)
import Data.Aeson (encode, ToJSON, FromJSON)

data APIError = APIError { code :: String, cause :: String } deriving (Generic, Show)

instance ToJSON APIError
instance FromJSON APIError

newtype Cause = Cause String

jsonResponse :: ToJSON a => Status -> a -> Response
jsonResponse status = responseLBS status [("Content-Type", "application/json")] . encode

jsonErrorResponse :: Status -> Cause -> Response
jsonErrorResponse status@Status { statusCode } (Cause cause) = responseLBS status [("Content-Type", "application/json")] . encode $ APIError (show statusCode) cause