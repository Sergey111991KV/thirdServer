module Adapter.HTTPWAI.Route where


import ClassyPrelude
-- import BasicPrelude
 
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP
import Domain.Types.ImportTypes
import Adapter.HTTPWAI.Cookie
import Adapter.HTTPWAI.Main
import  Network.Wai
import Domain.Services.ImportServices
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import BasicPrelude

type Router = [Text] 



data API
  = POST Router
  | GET Router
  | PUT Router
  | DELETE Router
  | UNKNOWN
  deriving (Show, Eq)


newtype Body = Body Int

instance ToJSON Body where
  toJSON (Body i) = object ["hello" .= i]
instance FromJSON Body where
  parseJSON = withObject "Body" $ \o -> Body <$> o .: "hello"

route :: (Monad m, CommonService m)
  => HTTP.Request -> m HTTP.Response
route req = do
  case methodAndPath req of
    Adapter.HTTPWAI.Route.GET  ["user"] -> do
        
        let sess = SessionId "OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v"
        -- get cookie  

        news <- getOneCommon sess UserEntReq 1
        return $ responseBuilder status200 [("Content-Type", "application/json")] $ fromEncoding $ toEncoding  (getData  news :: User )
       
    Adapter.HTTPWAI.Route.GET  ["user", idE] -> do
       
        let sess = SessionId "OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v"
        -- get cookie - 

        let unpackIdEntity = read  idE :: Int 
        news <- getOneCommon sess UserEntReq unpackIdEntity
        return $ responseBuilder status200 [("Content-Type", "application/json")] $ fromEncoding $ toEncoding  (getData  news :: User )
    Adapter.HTTPWAI.Route.GET  pass ->
      pure $ HTTP.responseLBS HTTP.status404 [] ""
    -- PUT (matches ["api", "user", ":pk"] -> Just [userId]) ->
    --   pure $ HTTP.responseLBS HTTP.status404 [] ""
    _ -> pure $ HTTP.responseLBS HTTP.status404 [] ""







methodAndPath :: HTTP.Request -> API
methodAndPath req =
  case getMethod of
    HTTP.POST -> Adapter.HTTPWAI.Route.POST $ HTTP.pathInfo req
    HTTP.GET -> Adapter.HTTPWAI.Route.GET $ HTTP.pathInfo req
    HTTP.PUT -> Adapter.HTTPWAI.Route.PUT $ HTTP.pathInfo req
    HTTP.DELETE -> Adapter.HTTPWAI.Route.DELETE $ HTTP.pathInfo req
    _ -> UNKNOWN
  where
    getMethod =
      either (error . show) id $ HTTP.parseMethod (HTTP.requestMethod req)
