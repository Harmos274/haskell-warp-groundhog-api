{-# LANGUAGE OverloadedStrings, Strict, DeriveGeneric, NamedFieldPuns #-}

module Core
  ( jsonResponse,
    jsonErrorResponse,
    json404NotFound,
    Cause (..),
    APIError (..),
  ) where

import Network.Wai (Response, responseLBS)
import Network.HTTP.Types (Status (..), notFound404)
import GHC.Generics (Generic)
import Data.Aeson (encode, ToJSON, FromJSON)

data APIError = APIError { code :: String, cause :: String } deriving (Generic, Show)

instance ToJSON APIError
instance FromJSON APIError

newtype Cause = Cause String

jsonResponse :: ToJSON a => Status -> a -> Response
jsonResponse status = responseLBS status [("Content-Type", "application/json")] . encode

jsonErrorResponse :: Status -> Cause -> Response
jsonErrorResponse status@Status { statusCode } (Cause cause) = jsonResponse status $ APIError (show statusCode) cause

json404NotFound :: Response
json404NotFound = jsonErrorResponse notFound404 (Cause "Resource not found.")