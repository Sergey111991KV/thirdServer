module Adapter.HTTP.API.Auth where

import Adapter.HTTP.ImportLibrary
  ( Auth
  , Either(Left, Right)
  , Log(..)
  , LogLevel(Debug, ErrorLog)
  , Login(Login)
  , MonadIO
  , Password(Password)
  , Text
  , ($)
  , errorText
  , lift
  , sessionByAuth
  , setDefaultCookie
  , setSessionIdInCookie
  , status200
  , status400
  )
import Web.Scotty.Trans (ScottyError, ScottyT, get, param, status)

routes :: (ScottyError e, MonadIO m, Auth m, Log m) => ScottyT e m ()
routes = do
  get "/api/auth/:loggin/:password" $ do
    loggin :: Text <- param "loggin"
    password :: Text <- param "password"
    authResult <- lift $ sessionByAuth (Login loggin) (Password password)
    case authResult of
      Left err -> do
        lift $ writeLog ErrorLog (errorText err)
        status status400
      Right sess -> do
        setSessionIdInCookie sess
        lift $ writeLog Debug "Good auth"
        status status200
  get "/api/auth/exit" $ do
    setDefaultCookie
    status status200
