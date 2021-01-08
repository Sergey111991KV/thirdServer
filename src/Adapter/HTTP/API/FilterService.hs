module Adapter.HTTP.API.FilterService where

import Adapter.HTTP.ImportLibrary

import qualified Prelude as P
import Web.Scotty.Trans (ScottyError, ScottyT, get, json, param, status)

routes ::
     (ScottyError e, MonadIO m, Auth m, Log m, CommonService m, FilterService m)
  => ScottyT e m ()
routes = do
  get "/api/news/filterOfData/:time/:condition" $ do
    time :: Text <- param "time"
    condition :: Text <- param "condition"
    getResult <- lift $ filterOfData (unpack time) (unpack condition)
    case getResult of
      Left err -> do
        lift $ writeLog ErrorLog (errorText err)
        status status400
      Right news -> do
        Web.Scotty.Trans.json news
  get "/api/news/filterAuthor/:name" $ do
    nameId :: Text <- param "name"
    getResult <- lift $ filterAuthor (P.read $ unpack nameId)
    case getResult of
      Left err -> do
        lift $ writeLog ErrorLog (errorText err)
        status status400
      Right news -> do
        Web.Scotty.Trans.json news
  get "/api/news/filterCategory/:idCat" $ do
    idCat :: Text <- param "idCat"
    getResult <- lift $ filterCategory  (P.read $ unpack idCat)
    case getResult of
      Left err -> do
        lift $ writeLog ErrorLog (errorText err)
        status status400
      Right news -> do
        Web.Scotty.Trans.json news
  get "/api/news/filterTeg/:idTag" $ do
    idTeg :: Text <- param "idTag"
    getResult <- lift $ filterTeg  (P.read $ unpack idTeg)
    case getResult of
      Left err -> do
        lift $ writeLog ErrorLog (errorText err)
        status status400
      Right news -> do
        Web.Scotty.Trans.json news
  get "/api/news/filterOneOfTags/:idTags" $ do
    idTegs :: Text <- param "idTags"
    getResult <- lift $ filterOneOfTags  (unpack idTegs)
    case getResult of
      Left err -> do
        lift $ writeLog ErrorLog (errorText err)
        status status400
      Right news -> do
        Web.Scotty.Trans.json news
  get "/api/news/filterAllOfTags/:idTags" $ do
    idTegs :: Text <- param "idTags"
    getResult <- lift $ filterAllOfTags (unpack idTegs)
    case getResult of
      Left err -> do
        lift $ writeLog ErrorLog (errorText err)
        status status400
      Right news -> do
        Web.Scotty.Trans.json news
  get "/api/news/filterName/:param" $ do
    paramFilterName :: Text <- param "param"
    getResult <- lift $ filterName (unpack paramFilterName)
    case getResult of
      Left err -> do
        lift $ writeLog ErrorLog (errorText err)
        status status400
      Right news -> do
        Web.Scotty.Trans.json news
  get "/api/news/filterContent/:param" $ do
    paramFilterContent :: Text <- param "param"
    getResult <- lift $ filterContent (unpack paramFilterContent)
    case getResult of
      Left err -> do
        lift $ writeLog ErrorLog (errorText err)
        status status400
      Right news -> do
        Web.Scotty.Trans.json news
