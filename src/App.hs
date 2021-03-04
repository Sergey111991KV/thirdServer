module App
  ( mainServer
  ) where

import qualified Adapter.PostgreSQL.ExportPostgres
                                               as Pos
import           ClassyPrelude                  ( ($)
                                                , Monad(return, (>>=))
                                                , Functor
                                                , Applicative(pure)
                                                , Int
                                                , IO
                                                , Either(..)
                                                , Text
                                                , either
                                                , print
                                                , getCurrentTime
                                                , try
                                                , newTVarIO
                                                , readTVarIO
                                                , MonadIO(..)
                                                , TVar
                                                , SomeException
                                                , MonadReader(..)
                                                , ReaderT(..)
                                                )



import           Control.Monad.Except           ( MonadError(throwError)
                                                , ExceptT
                                                , runExceptT
                                                )

import           Control.Monad.Catch            ( MonadThrow )
import qualified Data.Text.IO                  as TIO
import qualified Domain.Config.Config          as Config
import           Domain.Services.ExportServices ( Entity(..)
                                                , writeLogHandler
                                                , Log(..)
                                                , Auth(..)
                                                , Access(..)
                                                , SortedOfService(..)
                                                , FilterService(..)
                                                , CommonService(..)
                                                )
import           Domain.Types.ExportTypes       ( ErrorServer
                                                  ( ErrorReadFile
                                                  , ErrorGetConfig
                                                  )
                                                , LogLevel
                                                  ( Debug
                                                  , Error
                                                  , Warning
                                                  )
                                                )
import qualified Domain.Types.LogEntity.LogEntity
                                               as Log
import qualified Network.Wai.Handler.Warp      as W
import qualified Adapter.HTTPWAI.ExportHTTP    as MyHTTP
import qualified Domain.DomainEntityLogic.DomainEntityLogic
                                               as DomLog

type State = (Pos.State, TVar Log.StateLog)

newtype App a =
  App
    { unApp :: ReaderT State (ExceptT ErrorServer IO) a
    }
  deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, MonadThrow, MonadError ErrorServer )

runApp :: State -> App a -> IO (Either ErrorServer a)
runApp state app = runExceptT $ runReaderT (unApp app) state

instance Log App where
  writeLog logAp txtLog = do
    (_, st2) <- ask
    logSt    <- readTVarIO st2
    time     <- liftIO getCurrentTime
    liftIO $ writeLogHandler time (Log.logStCong logSt) logAp txtLog
  writeLogE = writeLog Error
  writeLogW = writeLog Warning
  writeLogD = writeLog Debug

instance Auth App where
  findUserId          = Pos.findUserId
  newSession          = Pos.newSession
  findUserIdBySession = Pos.findUserIdBySession
  deleteOldSession    = Pos.deleteOldSession

instance Access App where
  checkAuthorAccess = Pos.checkAuthorAccess
  checkAdminAccess  = Pos.checkAdminAccess
  getAuthorId       = Pos.getAuthorId


instance Entity App where
  fromAnEntity          = DomLog.fromAnEntity
  toAnEntity            = DomLog.toAnEntity
  toHelpForRequest      = DomLog.toHelpForRequest
  toQuantity            = DomLog.toQuantity
  getIntFromQueryArray  = DomLog.getIntFromQueryArray
  getTextFromQueryArray = DomLog.getTextFromQueryArray

instance CommonService App where
  create              = Pos.create
  editing             = Pos.editing
  getAll              = Pos.getAll
  getOne              = Pos.getOne
  remove              = Pos.remove
  editingAuthorAccess = Pos.editingAuthorAccess
  getAllAuthorAccess  = Pos.getAllAuthorAccess
  removeAuthorAccess  = Pos.removeAuthorAccess
  getOneAuthorAccess  = Pos.getOneAuthorAccess
  publish             = Pos.publish


instance SortedOfService App where
  sortedDate     = Pos.sortedDate
  sortedAuthor   = Pos.sortedAuthor
  sortedCategory = Pos.sortedCategory
  sortedPhoto    = Pos.sortedPhoto

instance FilterService App where
  filterOfData     = Pos.filterOfData
  filterAuthor     = Pos.filterAuthor
  filterCategory   = Pos.filterCategory
  filterTag        = Pos.filterTag
  filterOneOfTags  = Pos.filterOneOfTags
  filterAllOfTags  = Pos.filterAllOfTags
  filterName       = Pos.filterName
  filterContent    = Pos.filterContent
  filterAllContent = Pos.filterAllContent




withState :: Config.Config -> (Int -> State -> IO ()) -> IO ()
withState config action = do
  Pos.withState (Config.configPG config) $ \pgState -> do
    logState <- newTVarIO $ Config.configLog config
    let state = (pgState, logState)
    action (Config.configPort config) state


mainWithConfig :: Config.Config -> IO ()
mainWithConfig config = withState config $ \port state -> do
  W.run port $ \request respond -> do
    eitherResponse <- runApp state $ MyHTTP.route request
    response       <- either MyHTTP.serverErrorResponse pure eitherResponse
    respond response

mainServer :: IO ()
mainServer = do
  eitherConfig <- runExceptT $ getTextFromFile >>= getConfigFromText
  case eitherConfig of
    Left  e    -> print e
    Right conf -> mainWithConfig conf

getConfigFromText
  :: (MonadIO m, MonadError ErrorServer m) => Text -> m Config.Config
getConfigFromText txt = do
  caseOfConf <- liftIO $ Config.parseConf txt
  case caseOfConf of
    Left  _    -> throwError ErrorGetConfig
    Right conf -> return conf

getTextFromFile :: (MonadIO m, MonadError ErrorServer m) => m Text
getTextFromFile = do
  configFromFile :: Either SomeException Text <-
    liftIO $ ClassyPrelude.try $ TIO.readFile "server.config"
  case configFromFile of
    Left  _   -> throwError ErrorReadFile
    Right txt -> return txt

