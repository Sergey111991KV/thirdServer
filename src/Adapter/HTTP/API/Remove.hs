module Adapter.HTTP.API.Remove where

import Adapter.HTTP.ImportLibrary
 
import qualified Prelude as P
import Web.Scotty.Trans (ScottyError, ScottyT, delete, json, param, status)

routes ::
     (ScottyError e, MonadIO m, Auth m, Log m, CommonService m)
  => ScottyT e m ()
routes = do
  Web.Scotty.Trans.delete "/api/delete/:entity/:ide" $ do
    authResult <- getCookie "sId"
    case authResult of
      Nothing -> do
        status status400
      --   Web.Scotty.Trans.json ("not verification" :: Text)
      -- Just sess -> do
      --   entityText :: Text <- param "entity"
      --   idDr :: Text <- param "ide"
      --   result <-
      --     lift $
      --     removeCommon
      --       (SessionId sess)
      --       (convertTextToHelpRequest entityText)
      --       (P.read $ unpack idDr)
      --   case result of
      --     Left err -> do
      --       lift $ writeLog ErrorLog (errorText err)
      --       status status400
      --       Web.Scotty.Trans.json ("can't remove entity" :: Text)
      --     Right _ -> status status200
