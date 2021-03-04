module Domain.Services.AuthSpec where

import ClassyPrelude
import           Domain.Services.LogMonad      
import           Domain.Types.ExportTypes      
import           Domain.Services.EntityService  
import Control.Monad.Except
import Test.Hspec 
import Domain.Services.Auth


unimplemented :: a
unimplemented = error "unimplemented"



data Fixture m =
  Fixture
    { 
    _findUserId :: Login -> Password -> m  UserId
    , _newSession :: UserId -> m SessionId
    , _findUserIdBySession :: SessionId -> m UserId
    , _deleteOldSession :: UserId -> m  ()
    , _writeLog  :: LogServer -> Text -> m ()
    , _writeLogE :: Text -> m ()
    , _writeLogW :: Text -> m ()
    , _writeLogD :: Text -> m ()
    , _fromAnEntity :: AnEntity -> m HelpForRequest
    , _toAnEntity :: ByteString -> HelpForRequest -> m AnEntity
    , _toHelpForRequest :: Text -> m HelpForRequest
    , _toQuantity :: Text -> m Quantity
    , _getIntFromQueryArray :: [(Text, Maybe Text)] -> Text -> m Int
    , _getTextFromQueryArray :: [(Text, Maybe Text)] -> Text -> m Text
    }

emptyFixture :: Fixture m
emptyFixture =
  Fixture
    { 
    _findUserId = unimplemented
    , _newSession = unimplemented
    , _findUserIdBySession = unimplemented
    , _deleteOldSession = unimplemented
    , _writeLog  = unimplemented
    , _writeLogE = unimplemented
    , _writeLogW = unimplemented
    , _writeLogD = unimplemented
    , _fromAnEntity = unimplemented
    , _toAnEntity = const unimplemented
    , _toHelpForRequest = unimplemented
    , _toQuantity = unimplemented
    , _getIntFromQueryArray = const unimplemented
    , _getTextFromQueryArray = const unimplemented
    }

newtype App a =
  App
    { unApp :: ReaderT (Fixture (Either ErrorServer) ) (ExceptT ErrorServer IO) a
    }
  deriving (Applicative, Functor, Monad, MonadReader (Fixture (Either ErrorServer)), MonadIO, MonadError ErrorServer)

runApp :: Fixture (Either ErrorServer) -> App a -> IO (Either ErrorServer a)
runApp fixture app = do
   runExceptT $ runReaderT  (unApp  app) fixture


instance Entity App where
    
instance Log App where
  
instance Auth App where
    findUserId login pass = do
        func <- asks _findUserId  
        liftEither $ func login pass
    newSession userId = do
        func <- asks _newSession  
        liftEither $ func userId
    findUserIdBySession sess = do
        func <- asks _findUserIdBySession  
        liftEither $ func sess
    deleteOldSession userId = do
        func <- asks _deleteOldSession  
        liftEither $ func userId




spec :: Spec
spec = do
  describe "Create session" $ do
    it "should not create session because not find user from login and password" $ do
      let fixture =
            emptyFixture
              { _findUserId = \_ _ -> throwError DataErrorPostgreSQL
              } 
      runApp fixture (sessionByAuth (Login "Text") (Password "Text")) `shouldReturn`
          Left DataErrorPostgreSQL
    it "should create session " $ do
      let fixture =
            emptyFixture
              { _findUserId  = \ (Login "Text") (Password "Text") -> return $ UserId 1
              , _newSession  = \ (UserId 1) -> return $ SessionId "Some Text"
              } 
      runApp fixture (sessionByAuth (Login "Text") (Password "Text")) `shouldReturn`
          Right (SessionId "Some Text")
  describe "Delete session" $ do
    it "should not delete old session because not find user from sessionId" $ do
      let fixture =
            emptyFixture
              { _findUserIdBySession = \_  -> throwError DataErrorPostgreSQL
              }
      runApp fixture (exitSession (SessionId "Test")) `shouldReturn`
            Left DataErrorPostgreSQL
    it "should  delete old session" $ do
      let fixture =
            emptyFixture
              { _findUserIdBySession = \(SessionId "Test")  -> return (UserId 1)
              , _deleteOldSession = \ (UserId 1) -> return ()
              }
      runApp fixture (exitSession (SessionId "Test")) `shouldReturn`
           Right ()
  


