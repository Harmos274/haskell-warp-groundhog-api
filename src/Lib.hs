{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runAppOnPort,
  )
where

import Network.Wai (Request, Response, pathInfo, Application)
import Network.Wai.Handler.Warp (run)
import Database.Groundhog.Postgresql (Postgresql)

import Core (json404NotFound)
import Users (usersRouter)

runAppOnPort :: Int -> Postgresql -> IO ()
runAppOnPort port db = run port (application db)

application :: Postgresql -> Application
application db request respond = router db request >>= respond

router ::  Postgresql -> Request -> IO Response
router db req | pathInfo req == ["users"] = usersRouter db req
router _ _                                = return json404NotFound
