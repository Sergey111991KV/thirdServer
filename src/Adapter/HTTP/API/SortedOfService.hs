module Adapter.HTTP.API.SortedOfService where

import Adapter.HTTP.ImportLibrary
  ( Either(Left, Right)
  , Log(..)
  , LogLevel(ErrorLog)
  , MonadIO
  , SortedOfService(..)
  , Text
  , ($)
  , errorText
  , lift
  , status400
  )
import Web.Scotty.Trans (ScottyError, ScottyT, get, json, param, status)

routes :: (ScottyError e, MonadIO m, Log m, SortedOfService m) => ScottyT e m ()
routes = do
  get "/api/news/sortedNews/:condition" $ do
    condition :: Text <- param "condition"
    getResult <- lift $ sortedNews condition
    case getResult of
      Left err -> do
        lift $ writeLog ErrorLog (errorText err)
        status status400
      Right news -> do
        Web.Scotty.Trans.json news
