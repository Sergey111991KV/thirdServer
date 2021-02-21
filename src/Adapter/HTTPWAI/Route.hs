module Adapter.HTTPWAI.Route where

import           ClassyPrelude
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as HTTP
import qualified Prelude
import           Domain.Types.ExportTypes
import           Adapter.HTTPWAI.HelpFunction
import           Domain.Services.ExportServices

type Router = [Text]

data API
  = POST Router
  | GET Router
  | PUT Router
  | DELETE Router
  | UNKNOWN
  deriving (Show, Eq)

route
  :: (Monad m, CommonService m, MonadIO m, SortedOfService m, FilterService m)
  => HTTP.Request
  -> m HTTP.Response
route req = do
  either notAutorized autorized (getCookie req)
 where
  notAutorized ErrorGetCookie = do
    case methodAndPath req of
      GET ["auth", login, pass] -> do
        newSess <- sessionByAuth (Login login) (Password pass)
        setCookie newSess
      _ -> pure $ HTTP.responseLBS HTTP.status404 [] ""

  notAutorized err = serverErrorResponse err

  autorized sess = do
    case methodAndPath req of
      GET ["auth", "exit"] -> do
        exitSession sess
        return $ successResponse ("auth exit" :: Text)

      GET ["publish", idE] -> do
        let unpackIdEntity = Prelude.read $ unpack idE :: Int
        publishAction sess unpackIdEntity
        return $ successResponse ("publish news" :: Text)

      GET arr -> do
        liftIO $ print $ HTTP.queryString req
        let help = Prelude.head arr
        news <- getCommon sess
                          help
                          (HTTP.queryToQueryText $ HTTP.queryString req)
        return $ successResponse' news

      POST arr -> do
        help    <- toHelpForRequest $ Prelude.head arr
        reqBody <- liftIO $ HTTP.getRequestBodyChunk req
        ent     <- toAnEntity reqBody help
        createCommon sess ent
        return . successResponse $ "success request " ++ Prelude.head arr

      PUT arr -> do
        help    <- toHelpForRequest $ Prelude.head arr
        reqBody <- liftIO $ HTTP.getRequestBodyChunk req
        ent     <- toAnEntity reqBody help
        editingCommon sess ent
        return . successResponse $ "editing request " ++ Prelude.head arr

      DELETE [helpReq, idE] -> do
        help <- toHelpForRequest helpReq
        let unpackIdEntity = Prelude.read $ unpack idE :: Int
        removeCommon sess help unpackIdEntity
        return . successResponse $ "delete request" ++ helpReq
      _ -> pure $ HTTP.responseLBS HTTP.status404 [] ""


methodAndPath :: HTTP.Request -> API
methodAndPath req = case getMethod of
  HTTP.POST   -> Adapter.HTTPWAI.Route.POST $ HTTP.pathInfo req
  HTTP.GET    -> Adapter.HTTPWAI.Route.GET $ HTTP.pathInfo req
  HTTP.PUT    -> Adapter.HTTPWAI.Route.PUT $ HTTP.pathInfo req
  HTTP.DELETE -> Adapter.HTTPWAI.Route.DELETE $ HTTP.pathInfo req
  _           -> UNKNOWN
 where
  getMethod =
    either (error . show) id $ HTTP.parseMethod (HTTP.requestMethod req)
