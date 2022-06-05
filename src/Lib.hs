{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runAppOnPort,
  )
where

import Network.HTTP.Types (status404)
import Network.Wai (Application, Request, Response, pathInfo, responseBuilder, ResponseReceived)
import Network.Wai.Handler.Warp (run)
import Users (usersRouter)
import Data.Binary.Builder (empty)
import Database.Groundhog.Postgresql (PersistBackend (..))
import Database.Groundhog.Core (runDb')

runAppOnPort :: Int -> IO ()
runAppOnPort port = run port application 

application :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application request respond = router request >>= respond

router ::  PersistBackend m => Request -> m Response
router req | pathInfo req == ["users"] = usersRouter req
router _                               = return $ responseBuilder status404 [] empty
