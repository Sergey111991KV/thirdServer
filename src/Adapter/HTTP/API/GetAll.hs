module Adapter.HTTP.API.GetAll where

import Adapter.HTTP.ImportLibrary
 
import Web.Scotty.Trans (ScottyError, ScottyT, get, json, param, status)

routes ::
     (ScottyError e, MonadIO m, Auth m, Log m, CommonService m)
  => ScottyT e m ()
routes = do
  get "/api/getAll/:entity" $ do
    authResult <- getCookie "sId"
    case authResult of
      Nothing -> do
        status status400
        Web.Scotty.Trans.json ("not verification" :: Text)
      -- Just sess -> do
      --   entityText :: Text <- param "entity"
      --   case entityText of
      --     "author" -> do
      --       result <-
      --         lift $
      --         getEntityArray
      --           (SessionId sess)
      --           (convertTextToHelpRequest entityText)
      --       case result of
      --         Left err -> do
      --           lift $ writeLog ErrorLog (errorText err ++ " Get All Author")
      --           status status400
      --           Web.Scotty.Trans.json ("can't get All Author" :: Text)
      --         Right ent -> do
      --           lift $ writeLog Debug "Get All Author"
      --           Web.Scotty.Trans.json (convertFromEntityArray ent :: [Author])
      --           status status200
      --     "user" -> do
      --       result <-
      --         lift $
      --         getEntityArray
      --           (SessionId sess)
      --           (convertTextToHelpRequest entityText)
      --       case result of
      --         Left err -> do
      --           lift $ writeLog ErrorLog (errorText err ++ " Get All User")
      --           status status400
      --           Web.Scotty.Trans.json ("can't get All User" :: Text)
      --         Right ent -> do
      --           lift $ writeLog Debug "Get All User"
      --           Web.Scotty.Trans.json (convertFromEntityArray ent :: [User])
      --           status status200
      --     "tag" -> do
      --       result <-
      --         lift $
      --         getEntityArray
      --           (SessionId sess)
      --           (convertTextToHelpRequest entityText)
      --       case result of
      --         Left err -> do
      --           lift $ writeLog ErrorLog (errorText err ++ " Get All Tag")
      --           status status400
      --           Web.Scotty.Trans.json ("can't get All Tag" :: Text)
      --         Right ent -> do
      --           lift $ writeLog Debug "Get All Tag"
      --           Web.Scotty.Trans.json (convertFromEntityArray ent :: [Tag])
      --           status status200
      --     "news" -> do
      --       result <-
      --         lift $
      --         getEntityArray
      --           (SessionId sess)
      --           (convertTextToHelpRequest entityText)
      --       case result of
      --         Left err -> do
      --           lift $ writeLog ErrorLog (errorText err ++ " Get All News")
      --           status status400
      --           Web.Scotty.Trans.json ("can't get All News" :: Text)
      --         Right ent -> do
      --           lift $ writeLog Debug "Get All News"
      --           Web.Scotty.Trans.json (convertFromEntityArray ent :: [News])
      --           status status200
      --     "category" -> do
      --       result <-
      --         lift $
      --         getEntityArray
      --           (SessionId sess)
      --           (convertTextToHelpRequest entityText)
      --       case result of
      --         Left err -> do
      --           lift $ writeLog ErrorLog (errorText err ++ " Get All Category")
      --           status status400
      --           Web.Scotty.Trans.json ("can't get All Category" :: Text)
      --         Right ent -> do
      --           lift $ writeLog Debug "Get All Category"
      --           Web.Scotty.Trans.json (convertFromEntityArray ent :: [Category])
      --           status status200
      --     "draft" -> do
      --       result <-
      --         lift $
      --         getEntityArray
      --           (SessionId sess)
      --           (convertTextToHelpRequest entityText)
      --       case result of
      --         Left err -> do
      --           lift $ writeLog ErrorLog (errorText err ++ " Get All Draft")
      --           status status400
      --           Web.Scotty.Trans.json ("can't get All Draft" :: Text)
      --         Right ent -> do
      --           lift $ writeLog Debug "Get All Draft"
      --           Web.Scotty.Trans.json (convertFromEntityArray ent :: [Draft])
      --           status status200
      --     _ -> do
      --       lift $ writeLog ErrorLog "Error get all api adress"
      --       status status400
      --       Web.Scotty.Trans.json ("Error get all api adress" :: Text)
      --                           --  Хотел сначала так сделать - не получилось
      --   -- result <- lift $  getEntityArray (SessionId sess) (convertTextToHelpRequest entityText) 
      --   -- print result
      --   -- case result of
      --   --                 Left err -> do
      --   --                     lift $ writeLog ErrorLog (errorText err)
      --   --                     status status400
      --   --                     Web.Scotty.Trans.json ("can't get one entity" :: Text)
      --   --                 Right (ent :: [EntAuthor Author]) -> do
      --   --                     Web.Scotty.Trans.json    
      --   --                 Right [EntAuthor ent] -> do
      --   --                     Web.Scotty.Trans.json  (convertFromEntityArray [EntAuthor ent] :: [Author])
      --   --                     status status200  
      --   --                 Right [EntUser ent] -> do
      --   --                     Web.Scotty.Trans.json (convertFromEntityArray [EntUser ent] :: [User])
      --   --                     status status200  
      --   --                 Right [EntCategory ent] -> do
      --   --                     Web.Scotty.Trans.json (convertFromEntityArray [EntCategory ent] :: [Category])
      --   --                     status status200  
      --   --                 Right [EntTag ent] -> do
      --   --                     Web.Scotty.Trans.json (convertFromEntityArray [EntTag ent] :: [Tag])
      --   --                     status status200  
      --   --                 Right [EntComment ent] -> do
      --   --                     Web.Scotty.Trans.json (convertFromEntityArray [EntComment ent] :: [Comment])
      --   --                     status status200  
      --   --                 Right [EntNews ent] -> do
      --   --                     Web.Scotty.Trans.json (convertFromEntityArray [EntNews ent] :: [News])
      --   --                     status status200 
      --   --                 Right [EntDraft ent] -> do
      --   --                     Web.Scotty.Trans.json (convertFromEntityArray [EntDraft ent] :: [Draft])
      --   --                     status status200 
