module Adapter.HTTP.API.Publish where

import Adapter.HTTP.ImportLibrary

import qualified Prelude as P
import Web.Scotty.Trans 

routes ::
     (ScottyError e, MonadIO m, Auth m, Log m, CommonService m)
  => ScottyT e m ()
routes = do
  get "/api/publish/:idD" $ do
    authResult <- getCookie "sId"
    case authResult of
      Nothing -> do
        lift $ writeLog ErrorLog (errorText ErrorGetCookie)
        status status400
        Web.Scotty.Trans.json ("not verification" :: Text)
      Just sess -> do
        idDr :: Text <- param "idD"
        result <- lift $ publishAction (SessionId sess) (P.read $ unpack idDr)
        case result of
          Left err -> do
            lift $ writeLog ErrorLog (errorText err)
            status status400
            Web.Scotty.Trans.json ("can't create entity" :: Text)
          Right _ -> status status200
