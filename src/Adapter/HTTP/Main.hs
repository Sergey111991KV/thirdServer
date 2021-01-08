module Adapter.HTTP.Main where

import qualified Adapter.HTTP.API.Auth as AuthReppo
import qualified Adapter.HTTP.API.Create as Create
import qualified Adapter.HTTP.API.Editing as Edit
import qualified Adapter.HTTP.API.FilterService as FiltServ
import qualified Adapter.HTTP.API.GetAll as GetAll
import qualified Adapter.HTTP.API.GetOne as GetOne
import qualified Adapter.HTTP.API.Publish as Publish
import qualified Adapter.HTTP.API.Remove as Remove
import qualified Adapter.HTTP.API.SortedOfService as SortServ
import ClassyPrelude (IO, Int, LText, Monad((>>=)), MonadIO, Text, ($))
import Domain.Services.ImportServices as Service
  ( Auth
  , CommonService
  , FilterService
  , SortedOfService
  )
import Network.HTTP.Types.Status (status500)
import Network.Wai (Application, Response)
import Network.Wai.Handler.Warp (run)
import Web.Scotty.Trans (ScottyT, defaultHandler, json, scottyAppT, status)

app ::
     ( MonadIO m
     , Service.Auth m
     , Service.CommonService m
     , Service.FilterService m
     , Service.SortedOfService m
     )
  => (m Response -> IO Response)
  -> IO Application
app runner = scottyAppT runner routes

mainHTTP ::
     ( MonadIO m
     , Service.Auth m
     , Service.CommonService m
     , Service.FilterService m
     , Service.SortedOfService m
     )
  => Int
  -> (m Response -> IO Response)
  -> IO ()
mainHTTP port runner = app runner >>= run port

routes ::
     ( MonadIO m
     , Service.Auth m
     , Service.CommonService m
     , Service.FilterService m
     , Service.SortedOfService m
     )
  => ScottyT LText m ()
routes = do
  AuthReppo.routes
  Create.routes
  Edit.routes
  GetAll.routes
  GetOne.routes
  Publish.routes
  Remove.routes
  FiltServ.routes
  SortServ.routes
  defaultHandler $ \_ -> do
    status status500
    json ("InternalServerError" :: Text)



-- module Adapter.HTTP.Main where

-- import qualified Adapter.HTTP.API.Auth as AuthReppo
-- import qualified Adapter.HTTP.API.Create as Create
-- import qualified Adapter.HTTP.API.Editing as Edit
-- import qualified Adapter.HTTP.API.FilterService as FiltServ
-- import qualified Adapter.HTTP.API.GetAll as GetAll
-- import qualified Adapter.HTTP.API.GetOne as GetOne
-- import qualified Adapter.HTTP.API.Publish as Publish
-- import qualified Adapter.HTTP.API.Remove as Remove
-- import qualified Adapter.HTTP.API.SortedOfService as SortServ
-- import ClassyPrelude 
-- import Domain.Services.ImportServices as Service
--   ( Auth
--   , CommonService
--   , FilterService
--   , SortedOfService
--   )
-- import Network.HTTP.Types.Status (status500)
-- import Network.Wai (Application, Response)
-- import Network.Wai.Handler.Warp (run)
-- import Web.Scotty.Trans (ScottyT, defaultHandler, json, scottyAppT, status)
-- import Domain.Types.ImportTypes
-- import Control.Monad.Except

-- app ::
--      ( MonadIO m
--      , MonadError ErrorServer m
--      , Service.Auth m
--      , Service.CommonService m
--      , Service.FilterService m
--      , Service.SortedOfService m
--      )
--   => (m Response -> IO (Either ErrorServer  Response))
--   -> IO Application
-- app runner = undefined 
--   -- scottyAppT runner routes

-- mainHTTP ::
--      ( MonadIO m
--      , MonadError ErrorServer m
--      , Service.Auth m
--      , Service.CommonService m
--      , Service.FilterService m
--      , Service.SortedOfService m
--      )
--   => Int
--   -> (m Response -> IO (Either ErrorServer  Response))
--   -> IO ()
-- mainHTTP port runner = do
--   app runner >>= run port

-- routes ::
--      ( MonadIO m
--      , Service.Auth m
--      , Service.CommonService m
--      , Service.FilterService m
--      , Service.SortedOfService m
--      )
--   => ScottyT LText m ()
-- routes = do
--   AuthReppo.routes
--   Create.routes
--   Edit.routes
--   GetAll.routes
--   GetOne.routes
--   Publish.routes
--   Remove.routes
--   FiltServ.routes
--   SortServ.routes
--   defaultHandler $ \_ -> do
--     status status500
--     json ("InternalServerError" :: Text)