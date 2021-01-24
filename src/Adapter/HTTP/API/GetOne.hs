module Adapter.HTTP.API.GetOne where

import Adapter.HTTP.ImportLibrary
 
-- import qualified Prelude as P
import Web.Scotty.Trans 

routes ::
     (ScottyError e, MonadIO m, Auth m, Log m, CommonService m)
  => ScottyT e m ()
routes = do
  get "/api/getOne/:entity/:ide" $ do
    _ <- getCookie "sId"
    -- case authResult of
    --   Nothing -> do
    status status400
        -- Web.Scotty.Trans.json ("not verification" :: Text)
      -- Just sess -> do
      --   entityText :: Text <- param "entity"
      --   idDr :: Text <- param "ide"
      --   result <-
      --     lift $
      --     getEntityCommon
      --       (SessionId sess)
      --       (convertTextToHelpRequest entityText)
      --       (P.read $ unpack idDr)
      --   case result of
      --     Left err -> do
      --       lift $ writeLog ErrorLog (errorText err)
      --       status status400
      --       Web.Scotty.Trans.json ("can't get one entity" :: Text)
      --     Right (EntAuthor ent) -> do
      --       Web.Scotty.Trans.json ent
      --       status status200
      --     Right (EntUser ent) -> do
      --       Web.Scotty.Trans.json ent
      --       status status200
      --     Right (EntCategory ent) -> do
      --       Web.Scotty.Trans.json ent
      --       status status200
      --     Right (EntTag ent) -> do
      --       Web.Scotty.Trans.json ent
      --       status status200
      --     Right (EntComment ent) -> do
      --       Web.Scotty.Trans.json ent
      --       status status200
      --     Right (EntNews ent) -> do
      --       Web.Scotty.Trans.json ent
      --       status status200
      --     Right (EntDraft ent) -> do
      --       Web.Scotty.Trans.json ent
      --       status status200
