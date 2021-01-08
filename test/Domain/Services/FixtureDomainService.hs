module Domain.Services.FixtureDomainService where

import ClassyPrelude
import Fixture
import Domain.Types.ImportTypes
import Domain.Services.LogMonad 

newtype App a = App
  { unApp :: ReaderT (Fixture IO) IO a
  } deriving ( Applicative, Functor, Monad, MonadReader (Fixture IO), MonadIO
             )

runApp :: Fixture IO -> App a -> IO a
runApp fixture action = do
  flip runReaderT fixture . unApp $ action 


data Fixture m = Fixture
    { _findUserId :: Login -> Password -> m (Either ErrorServer UserId)
    , _newSession :: UserId -> m (Either ErrorServer SessionId)
    , _findUserIdBySession :: SessionId -> m (Either ErrorServer UserId)
    , _deleteOldSession :: UserId -> m (Either ErrorServer ())
    , _create :: Entity -> m (Either ErrorServer ())
    , _editing :: Entity -> m (Either ErrorServer ())
    , _remove :: HelpForRequest -> Int -> m (Either ErrorServer ())
    , _remove' :: HelpForRequest -> UserId -> Int -> m (Either ErrorServer ())
    , _getAll ::  HelpForRequest -> m (Either ErrorServer [Entity])
    , _getAllDraft :: UserId -> m (Either ErrorServer [Entity])
    , _getOne ::  HelpForRequest -> Int -> m (Either ErrorServer Entity)
    , _getOneDraft :: SessionId  -> Int -> m (Either ErrorServer Entity)
    , _publish :: UserId ->  Int -> m (Either ErrorServer ())
    , _checkAuthorAccess :: SessionId  -> m (Either ErrorServer Bool)  
    , _checkAdminAccess :: SessionId ->  m (Either ErrorServer Bool)
    }





emptyFixture :: Fixture m
emptyFixture = Fixture
    { _findUserId = const unimplemented
    , _newSession = const unimplemented
    , _findUserIdBySession = const unimplemented
    , _deleteOldSession = const unimplemented
    , _create  = const unimplemented
    , _editing  = const unimplemented
    , _remove  = const unimplemented
    , _remove' = const unimplemented
    , _getAll  = const unimplemented
    , _getAllDraft  = const unimplemented
    , _getOneDraft = const unimplemented
    , _getOne  = const unimplemented
    , _publish  = const unimplemented
    , _checkAuthorAccess  = const unimplemented 
    , _checkAdminAccess  = const unimplemented
    }


instance Log App where
    writeLog l txtLog = liftIO $ writeLogginHandler logConfTest l txtLog


logConfTest :: LogConfig
logConfTest = LogConfig {
    logFile = "log-journalTest.txt"
    , logLevelForFile = Debug
    , logConsole = True
    }

