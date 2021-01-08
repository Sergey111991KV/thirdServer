module Lib
  ( mainServer
  ) where

import qualified Adapter.HTTP.Main as HTTP
import qualified Adapter.PostgreSQL.ImportPostgres as Pos
import ClassyPrelude
 
import Control.Monad.Catch (MonadThrow)
import qualified Data.Text.IO as TIO
import qualified Domain.Config.Config as Config
import Domain.Services.ImportServices
  ( Auth(..)
  , CommonService(..)
  , FilterService(..)
  , Log(..)
  , SortedOfService(..)
  , writeLogginHandler
  )

import qualified  Prelude
import Domain.Types.ImportTypes 
import qualified Domain.Types.LogEntity.LogEntity as Log

type State = (Pos.State, TVar Log.StateLog)

newtype App a =
  App
    { unApp :: ReaderT State IO a
    }
  deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, MonadThrow)

run :: State -> App a -> IO a
run state = flip runReaderT state . unApp

instance Log App where
  writeLog logAp txtLog = do
    (_, st2) <- ask
    logSt <- readTVarIO st2
    liftIO $ writeLogginHandler (Log.logStCong logSt) logAp txtLog

instance Auth App where
  findUserId = Pos.findUserId
  newSession = Pos.newSession
  findUserIdBySession = Pos.findUserIdBySession
  deleteOldSession = Pos.deleteOldSession

instance CommonService App where
  create = Pos.create
  editing = Pos.editing
  getAll = Pos.getAll
  getOne = Pos.getOne
  remove = Pos.remove
  getAllDraft = Pos.getAllDraft
  removeDraft = Pos.removeDraft
  getOneDraft = Pos.getOneDraft
  publish = Pos.publish
  checkAuthorAccess = Pos.checkAuthorAccess
  checkAdminAccess = Pos.checkAdminAccess

instance SortedOfService App where
  sortedNews = Pos.sortedNews

instance FilterService App where
  filterOfData = Pos.filterOfData
  filterAuthor = Pos.filterAuthor
  filterCategory = Pos.filterCategory
  filterTeg = Pos.filterTeg
  filterOneOfTags = Pos.filterOneOfTags
  filterAllOfTags = Pos.filterAllOfTags
  filterName = Pos.filterName
  filterContent = Pos.filterContent

withState :: Config.Config -> (Int -> State -> IO ()) -> IO ()
withState config action = do
  Pos.withState (Config.configPG config) $ \pgState -> do
    logState <- newTVarIO $ Config.configLog config
    let state = (pgState, logState)
    action (Config.configPort config) state

mainWithConfig :: Config.Config -> IO ()
mainWithConfig config =
  withState config $ \port state -> do
    let runner = run state
    HTTP.mainHTTP port runner

startServer :: Text -> IO ()
startServer textFromFile = do
  caseOfConf <- Config.parseConf textFromFile
  case caseOfConf of
    Left err -> do
      print (errorText err ++ "take option for servet")
    Right conf -> do
      resultStart <- ClassyPrelude.try $ mainWithConfig conf
      case (resultStart :: Either SomeException ()) of
        Left exep -> print exep
        Right _ -> return ()

mainServer :: IO ()
mainServer = do
  configFromFile <- ClassyPrelude.try $ TIO.readFile "server.config"
  case (configFromFile :: Either SomeException Text) of
    Right config -> startServer config
    Left exep -> print exep


-- {-# LANGUAGE MultiParamTypeClasses,AllowAmbiguousTypes #-}

-- module Lib
--   ( mainServer
--   ) where

-- import qualified Adapter.HTTP.Main as HTTP
-- import qualified Adapter.PostgreSQL.ImportPostgres as Pos
-- import ClassyPrelude
 
-- import Control.Monad.Catch (MonadThrow)
-- import qualified Data.Text.IO as TIO
-- import qualified Domain.Config.Config as Config
-- import Domain.Services.ImportServices
--   ( Auth(..)
--   , CommonService(..)
--   , FilterService(..)
--   , Log(..)
--   , SortedOfService(..)
--   , writeLogginHandler
--   )

-- import qualified  Prelude
-- import Domain.Types.ImportTypes 
-- import qualified Domain.Types.LogEntity.LogEntity as Log
-- import Control.Monad.Except

-- type State = (Pos.State, TVar Log.StateLog)

-- newtype App a =
--   App
--     { unApp :: ReaderT State (ExceptT ErrorServer IO) a
--     }
--   deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, MonadThrow)

-- run :: State -> App a -> IO (Either ErrorServer a)
-- run state app = runExceptT $ runReaderT  (unApp  app) state

-- instance Log App where
--   writeLog logAp txtLog = do
--     (_, st2) <- ask
--     logSt <- readTVarIO st2
--     liftIO $ writeLogginHandler (Log.logStCong logSt) logAp txtLog

-- instance Auth App where
--   findUserId = Pos.findUserId
--   newSession = Pos.newSession
--   findUserIdBySession = Pos.findUserIdBySession
--   deleteOldSession = Pos.deleteOldSession

-- instance CommonService App where
--   create = Pos.create
--   editing = Pos.editing
--   getAll = Pos.getAll
--   getOne = Pos.getOne
--   remove = Pos.remove
--   getAllDraft = Pos.getAllDraft
--   removeDraft = Pos.removeDraft
--   getOneDraft = Pos.getOneDraft
--   publish = Pos.publish
--   checkAuthorAccess = Pos.checkAuthorAccess
--   checkAdminAccess = Pos.checkAdminAccess

-- instance SortedOfService App where
--   sortedNews = Pos.sortedNews

-- instance FilterService App where
--   filterOfData = Pos.filterOfData
--   filterAuthor = Pos.filterAuthor
--   filterCategory = Pos.filterCategory
--   filterTeg = Pos.filterTeg
--   filterOneOfTags = Pos.filterOneOfTags
--   filterAllOfTags = Pos.filterAllOfTags
--   filterName = Pos.filterName
--   filterContent = Pos.filterContent

-- withState :: Config.Config -> (Int -> State -> IO ()) -> IO ()
-- withState config action = do
--   Pos.withState (Config.configPG config) $ \pgState -> do
--     logState <- newTVarIO $ Config.configLog config
--     let state = (pgState, logState)
--     action (Config.configPort config) state

-- mainWithConfig :: Config.Config -> IO ()
-- mainWithConfig config =
--   withState config $ \port state -> do
--     let runner =  run state
--     HTTP.mainHTTP port  runner

-- -- instance  MonadError ErrorServer App

-- startServer :: Text -> IO ()
-- startServer textFromFile = do
--   caseOfConf <- Config.parseConf textFromFile
--   case caseOfConf of
--     Left err -> do
--       print (errorText err ++ "take option for servet")
--     Right conf -> do
--       resultStart <- ClassyPrelude.try $ mainWithConfig conf
--       case (resultStart :: Either SomeException ()) of
--         Left exep -> print exep
--         Right _ -> return ()

-- mainServer :: IO ()
-- mainServer = do
--   configFromFile <- ClassyPrelude.try $ TIO.readFile "server.config"
--   case (configFromFile :: Either SomeException Text) of
--     Right config -> startServer config
--     Left exep -> print exep