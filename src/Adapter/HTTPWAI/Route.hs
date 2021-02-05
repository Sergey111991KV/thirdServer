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



successResponse :: forall a. ToJSON a  => a  -> Response
successResponse  b = responseBuilder HTTP.status200 [("Content-Type", "application/json")] $ fromEncoding $ toEncoding  b

route :: (Monad m, CommonService m, MonadIO m)
  => HTTP.Request -> m HTTP.Response
route req = do
  case methodAndPath req of   
    GET  ["user", idE] -> do
       
        let sess = SessionId "OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v"

        let unpackIdEntity = read  idE :: Int 
        news <- getOneCommon sess UserEntReq unpackIdEntity
        return $ successResponse  (getData  news :: User )
    GET  ["user", idE] -> do
       
        let sess = SessionId "OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v"

        let unpackIdEntity = read  idE :: Int 
        news <- getOneCommon sess UserEntReq unpackIdEntity
        return $ successResponse  (getData  news :: User )
        
    PUT pass -> do
      reqBody <- liftIO $ HTTP.getRequestBodyChunk req
      
      return $ responseBuilder HTTP.status200 [("Content-Type", "application/json")] ""


    _ -> pure $ HTTP.responseLBS HTTP.status404 [] ""

-- news/filterAllOfTags/[int]
-- /news/filterAuthor/2
-- /news/filterCategory/1
-- /news/filterContent/descrip
-- /news/filterName/news
-- /news/filterOfData/less/2011-08-01
-- /news/filterOneOfTags/1,2
-- /news/filterTeg/1
-- authors
-- author
-- categorys
-- category
-- drafts
-- draft
-- news_s
-- news
-- tags
-- tag
-- users
-- user
-- publish -- GET
-- news/sortedNews/author
-- news/sortedNews/category
-- news/sortedNews/date
-- news/sortedNews/photo





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
