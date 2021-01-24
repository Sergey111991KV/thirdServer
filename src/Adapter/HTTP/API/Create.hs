module Adapter.HTTP.API.Create where

import Adapter.HTTP.ImportLibrary
 
import Web.Scotty.Trans 

routes ::
     (ScottyError e, MonadIO m, Auth m, Log m, CommonService m)
  => ScottyT e m ()
routes = do
  post "/api/create/:entity" $ do
    _ <- getCookie "sId"
    -- case authResult of
    --   Nothing -> do
    --     lift $ writeLog ErrorLog (errorText ErrorGetCookie)
    status status400
      --   Web.Scotty.Trans.json ("not verification" :: Text)
      -- Just sess -> do
      --   b <- body
      --   entityConvert :: Text <- param "entity"
      --   print (funcDecodeToEntity entityConvert b)
      --   result <-
      --     lift $
      --     createCommon (SessionId sess) (funcDecodeToEntity entityConvert b)
      --   case result of
      --     Left err -> do
      --       lift $ writeLog ErrorLog (errorText err)
      --       status status400
      --       Web.Scotty.Trans.json ("can't create entity" :: Text)
      --     Right _ -> status status200
