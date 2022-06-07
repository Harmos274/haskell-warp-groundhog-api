{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runAppOnPort,
  )
where

import Network.HTTP.Types (status404)
import Network.Wai (Request, Response, pathInfo, responseBuilder, Application)
import Network.Wai.Handler.Warp (run)
import Users (usersRouter)
import Data.Binary.Builder (empty)
import Database.Groundhog.Postgresql (Postgresql)

runAppOnPort :: Int -> Postgresql -> IO ()
runAppOnPort port db = run port (application db)

application :: Postgresql -> Application
application db request respond = router db request >>= respond

router ::  Postgresql -> Request -> IO Response
router db req | pathInfo req == ["users"] = usersRouter db req
router _ _                                = return $ responseBuilder status404 [] empty
