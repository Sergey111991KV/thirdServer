module Domain.Services.CommonServiceSpec where

import ClassyPrelude
import qualified Prelude as P
import           Domain.Services.LogMonad      
import Domain.Types.ExportTypes

import           Domain.Services.EntityService  
import Control.Monad.Except
import Test.Hspec 
import qualified Data.ByteString.Lazy.Internal as LB
import Domain.Services.Auth
import Domain.Services.AccessService 
import Domain.Services.CommonService 
import Domain.Services.FilterService
import Domain.Services.SortedOfService
import qualified Domain.DomainEntityLogic.DomainEntityLogic as DL



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
    , _checkAuthorAccess :: SessionId -> m  ()
    , _checkAdminAccess :: SessionId -> m  ()
    , _getAuthorId :: SessionId -> m Int
    , _create :: AnEntity -> m  ()
    , _editing :: AnEntity -> m  ()
    , _editingAuthorAccess :: AnEntity -> UserId -> m  ()
    , _remove :: HelpForRequest -> Int -> m  ()
    , _removeAuthorAccess :: Int -> UserId ->  m  ()
    , _getAll :: HelpForRequest -> Int -> m  LB.ByteString
    , _getAllAuthorAccess :: UserId -> Int -> m LB.ByteString
    , _getOne :: HelpForRequest -> Int -> m  LB.ByteString
    , _getOneAuthorAccess :: Int -> UserId -> m  LB.ByteString
    , _publish :: UserId -> Int -> m  ()

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
    , _checkAuthorAccess  = unimplemented
    , _checkAdminAccess  = unimplemented
    , _getAuthorId = unimplemented
    , _create  = unimplemented
    , _editing  = unimplemented
    , _editingAuthorAccess = const unimplemented
    , _remove = const unimplemented
    , _removeAuthorAccess = const unimplemented
    , _getAll = const unimplemented
    , _getAllAuthorAccess = const unimplemented
    , _getOne = const unimplemented
    , _getOneAuthorAccess = const unimplemented
    , _publish = const unimplemented
    }

newtype App a =
  App
    { unApp :: ReaderT (Fixture (Either ErrorServer) ) (ExceptT ErrorServer IO) a
    }
  deriving (Applicative, Functor, Monad, MonadReader (Fixture (Either ErrorServer)), MonadIO, MonadError ErrorServer)

runApp :: Fixture (Either ErrorServer) -> App a -> IO (Either ErrorServer a)
runApp fixture app = do
   runExceptT $ runReaderT  (unApp  app) fixture

instance Access App where
  checkAdminAccess  sess = do
      func <- asks _checkAdminAccess  
      liftEither $ func sess
  checkAuthorAccess sess = do
      func <- asks _checkAuthorAccess  
      liftEither $ func sess
  getAuthorId sess = do
      func <- asks _getAuthorId  
      liftEither $ func sess
    


instance FilterService App where

instance SortedOfService App where

instance CommonService App where
    create ent = do
      func <- asks _create  
      liftEither $ func ent
    editing ent = do
      func <- asks _create  
      liftEither $ func ent

    -- editingAuthorAccess :: AnEntity -> UserId -> m  ()
    -- remove :: HelpForRequest -> Int -> m  ()
    -- removeAuthorAccess :: Int -> UserId ->  m  ()
    -- getAll :: HelpForRequest -> Int -> m  LB.ByteString
    -- getAllAuthorAccess :: UserId -> Int -> m LB.ByteString
    -- getOne :: HelpForRequest -> Int -> m  LB.ByteString
    -- getOneAuthorAccess :: Int -> UserId -> m  LB.ByteString
    publish userId draftId = do
        func <- asks _publish  
        liftEither $ func userId draftId 

instance Entity App where
    fromAnEntity = DL.fromAnEntity
    -- toAnEntity :: ByteString -> HelpForRequest -> m AnEntity
    -- toHelpForRequest :: Text -> m HelpForRequest
    -- toQuantity :: Text -> m Quantity
    -- getIntFromQueryArray :: [(Text, Maybe Text)] -> Text -> m Int
    -- getTextFromQueryArray :: [(Text, Maybe Text)] -> Text -> m Text


    
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

someTime :: UTCTime
someTime =  P.read "2011-11-19 18:28:52.607875 UTC" :: UTCTime

mockAuthor :: AnEntity 
mockAuthor = AnAuthor (Author 1 (UserId 1) "Some description")

mockDraft :: AnEntity 
mockDraft = AnDraft (Draft 1 "text draft" someTime Nothing  "text draft" "text draft" ["text draft"] [1] 1)

mockDraftNotAuthor :: AnEntity 
mockDraftNotAuthor = AnDraft (Draft 2 "text draft" someTime Nothing  "text draft" "text draft" ["text draft"] [1] 2)


mockUser :: AnEntity 
mockUser = AnUser (User (UserId 2) "Text" "Text" (Login "Login") (Password "Password") "Text" someTime True True )

mockTag :: AnEntity 
mockTag = AnTag (Tag 1 "Tag")

-- mockNews :: AnEntity 
-- mockNews = AnNews News

--  (News 1 
--                         someTime  
--                         (Author 1 (UserId 1) "Some description") 
--                         (Category 1 "Category Test" Nothing)
--                         "textNewsRaw"
--                         "mainPhotoUrlNewsRaw"
--                         ["otherPhotoUrlNewsRaw"]
--                         "shortNameNewsRaw"
--                         [Comment 1 "Text" someTime 1 (UserId 1)]
--                         [Tag 1 "Tag"]
--                         )
  
  --  AnNews (News   1 
  --                         someTime 
  --                         (Author 1 (UserId 1) "Some description") 
  --                         (Category 1 "Category Test" Nothing)
  --                         "textNewsRaw"
  --                         "mainPhotoUrlNewsRaw"
  --                         ["otherPhotoUrlNewsRaw"]
  --                         "shortNameNewsRaw"
  --                         []
  --                         []
  --                   )

mockComment :: AnEntity 
mockComment = AnComment (Comment 1 "Text" someTime 1 (UserId 1))

mockCategory :: AnEntity 
mockCategory = AnCategory (Category 1 "Category Test" Nothing)




spec :: Spec
spec = do
  describe "Publish" $ do
    it "should not Publish news because not find user from sessionId" $ do
      let fixture =
            emptyFixture
              { _findUserIdBySession = \_  -> throwError DataErrorPostgreSQL
              } 
      runApp fixture (publishAction (SessionId "Text") 1) `shouldReturn`
        Left DataErrorPostgreSQL
    it "should Publish news" $ do
      let fixture =
            emptyFixture
              { _findUserIdBySession = \(SessionId "Text") -> return $ UserId 1
              , _publish  = \(UserId 1) 1 -> return ()
              } 
      runApp fixture (publish (UserId 1) 1) `shouldReturn`
        Right ()
    it "should not Publish news because wrong userId as not support author in the database" $ do
      let fixture =
            emptyFixture
              { _findUserIdBySession = \(SessionId "Text") -> return $ UserId 1
              , _publish  = \(UserId 1) 1 -> throwError DataErrorPostgreSQL
              } 
      runApp fixture (publish (UserId 1) 1) `shouldReturn`
        Left DataErrorPostgreSQL
  describe "CreateCommon" $ do
    it "should not createCommon with error in postgres" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \_  -> throwError DataErrorPostgreSQL
              } 
      runApp fixture (createCommon (SessionId "Text") mockAuthor) `shouldReturn`
          Left DataErrorPostgreSQL
    it "should not create Author with not admin access" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \_  -> throwError NotAccessNotAdmid
              } 
      runApp fixture (createCommon (SessionId "Text") mockAuthor) `shouldReturn`
          Left NotAccessNotAdmid
    it "should not create Category with not admin access" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \_  -> throwError NotAccessNotAdmid
              } 
      runApp fixture (createCommon (SessionId "Text") mockCategory) `shouldReturn`
          Left NotAccessNotAdmid
    it "should not create Tag with not admin access" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \_  -> throwError NotAccessNotAdmid
              } 
      runApp fixture (createCommon (SessionId "Text") mockTag) `shouldReturn`
          Left NotAccessNotAdmid
    it "should not create Draft with not author access" $ do
      let fixture =
            emptyFixture
              { _checkAuthorAccess = \_  -> throwError NotAccessNotAuthor
              } 
      runApp fixture (createCommon (SessionId "Text") mockDraft) `shouldReturn`
          Left NotAccessNotAuthor
    it "should create Author with admin access" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \(SessionId "Text")   -> return ()
               , _create = \_ -> return ()
              } 
      runApp fixture (createCommon (SessionId "Text") mockAuthor) `shouldReturn`
          Right ()
    it "should create Category with admin access" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \(SessionId "Text")   -> return ()
               , _create = \_ -> return ()
              } 
      runApp fixture (createCommon (SessionId "Text") mockCategory) `shouldReturn`
         Right ()
    it "should create Tag with admin access" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \(SessionId "Text")   -> return ()
              , _create = \_ -> return ()
              } 
      runApp fixture (createCommon (SessionId "Text") mockTag) `shouldReturn`
          Right ()
    it "should create Draft with author access" $ do
      let fixture =
            emptyFixture
              { _checkAuthorAccess = \(SessionId "Text")   -> return ()
              , _getAuthorId = \(SessionId "Text")   -> return 1
              , _create = do
                  \ent   -> do
                    case ent of
                      mockDraft -> return ()
              } 
      runApp fixture (createCommon (SessionId "Text") mockDraft) `shouldReturn`
          Right ()
    it "should not create Draft, because it is not this author draft" $ do
      let fixture =
            emptyFixture
              { _checkAuthorAccess = \(SessionId "Text")   -> return ()
              , _getAuthorId = \(SessionId "Text")   -> return 5
              , _create = do
                  \ent   -> do
                    case ent of
                      mockDraftNotAuthor  -> return ()
              } 
      runApp fixture (createCommon (SessionId "Text") mockDraftNotAuthor) `shouldReturn`
          Left NotSupposedAuthor
  describe "editingCommon" $ do
    it "should not editingCommon with error in postgres" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \_  -> throwError DataErrorPostgreSQL
              } 
      runApp fixture (editingCommon (SessionId "Text") mockAuthor) `shouldReturn`
          Left DataErrorPostgreSQL
    it "should not editing Author with not admin access" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \_  -> throwError NotAccessNotAdmid
              } 
      runApp fixture (editingCommon (SessionId "Text") mockAuthor) `shouldReturn`
          Left NotAccessNotAdmid
    it "should not editing Category with not admin access" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \_  -> throwError NotAccessNotAdmid
              } 
      runApp fixture (editingCommon (SessionId "Text") mockCategory) `shouldReturn`
          Left NotAccessNotAdmid
    it "should not editing Tag with not admin access" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \_  -> throwError NotAccessNotAdmid
              } 
      runApp fixture (editingCommon (SessionId "Text") mockTag) `shouldReturn`
          Left NotAccessNotAdmid
    it "should not editing Draft with not author access" $ do
      let fixture =
            emptyFixture
              { _checkAuthorAccess = \_  -> throwError NotAccessNotAuthor
              } 
      runApp fixture (editingCommon (SessionId "Text") mockDraft) `shouldReturn`
          Left NotAccessNotAuthor
    it "should editing Author with admin access" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \(SessionId "Text")   -> return ()
              , _editing = \_ -> return ()
              } 
      runApp fixture (editingCommon (SessionId "Text") mockAuthor) `shouldReturn`
          Right ()
    it "should editing Category with admin access" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \(SessionId "Text")   -> return ()
              , _editing = \_ -> return ()
              } 
      runApp fixture (editingCommon (SessionId "Text") mockCategory) `shouldReturn`
         Right ()
    it "should editing Tag with admin access" $ do
      let fixture =
            emptyFixture
              { _checkAdminAccess = \(SessionId "Text")   -> return ()
              , _editing = \_ -> return ()
              } 
      runApp fixture (editingCommon (SessionId "Text") mockTag) `shouldReturn`
          Right ()
    it "should editing Draft with author access" $ do
      let fixture =
            emptyFixture
              { _checkAuthorAccess = \(SessionId "Text")   -> return ()
              , _getAuthorId = \(SessionId "Text")   -> return 1
              , _editing = \_ -> return ()
              } 
      runApp fixture (editingCommon (SessionId "Text") mockDraft) `shouldReturn`
          Right ()
    it "should not editing Draft, because it is not this author draft" $ do
      let fixture =
            emptyFixture
              { _checkAuthorAccess = \(SessionId "Text")   -> return ()
              , _getAuthorId = \(SessionId "Text")   -> return 5
              , _editing = \_ -> return ()
              } 
      runApp fixture (editingCommon (SessionId "Text") mockDraftNotAuthor) `shouldReturn`
          Left NotSupposedAuthor
   


-- editingCommon :: CommonService m => SessionId -> AnEntity -> m ()
-- editingCommon sess ent = do
--   helpR <- fromAnEntity ent
--   case helpR of
--     AuthorEntReq   -> checkAdminAccess sess >> editing ent
--     UserEntReq     -> checkAdminAccess sess >> editing ent
--     TagEntReq      -> checkAdminAccess sess >> editing ent
--     CommentEntReq  -> editing ent
--     CategoryEntReq -> checkAdminAccess sess >> editing ent
--     DraftEntReq ->
--       checkAuthorAccess sess
--         >>  findUserIdBySession sess
--         >>= editingAuthorAccess ent
--     _ -> throwError NotTakeEntity

-- removeCommon :: CommonService m => SessionId -> HelpForRequest -> Int -> m ()
-- removeCommon sess helpReq idEnt = do
--   case helpReq of
--     AuthorEntReq   -> checkAdminAccess sess >> remove helpReq idEnt
--     UserEntReq     -> checkAdminAccess sess >> remove helpReq idEnt
--     TagEntReq      -> checkAdminAccess sess >> remove helpReq idEnt
--     CommentEntReq  -> remove helpReq idEnt
--     CategoryEntReq -> checkAdminAccess sess >> remove helpReq idEnt
--     DraftEntReq ->
--       checkAuthorAccess sess
--         >>  findUserIdBySession sess
--         >>= removeAuthorAccess idEnt
--     NewsEntReq -> remove helpReq idEnt
--     _          -> throwError NotTakeEntity

-- getOneCommon
--   :: CommonService m => SessionId -> HelpForRequest -> Int -> m LB.ByteString
-- getOneCommon sess helpReq idE = do
--   case helpReq of
--     AuthorEntReq   -> checkAdminAccess sess >> getOne helpReq idE
--     UserEntReq     -> getOne helpReq idE
--     TagEntReq      -> getOne helpReq idE
--     CommentEntReq  -> getOne helpReq idE
--     CategoryEntReq -> getOne helpReq idE
--     DraftEntReq ->
--       checkAuthorAccess sess
--         >>  findUserIdBySession sess
--         >>= getOneAuthorAccess idE
--     NewsEntReq -> getOne helpReq idE
--     _          -> throwError NotTakeEntity

-- getArrayCommon
--   :: CommonService m => SessionId -> HelpForRequest -> Int -> m LB.ByteString
-- getArrayCommon sess helpReq page = do
--   case helpReq of
--     AuthorEntReq   -> checkAdminAccess sess >> getAll helpReq page
--     UserEntReq     -> checkAdminAccess sess >> getAll helpReq page
--     TagEntReq      -> getAll helpReq page
--     CommentEntReq  -> getAll helpReq page
--     CategoryEntReq -> getAll helpReq page
--     DraftEntReq    -> do
--       checkAuthorAccess sess
--       uId <- findUserIdBySession sess
--       getAllAuthorAccess uId page
--     NewsEntReq -> getAll helpReq page
--     _          -> throwError NotTakeEntity

-- getCommon
--   :: CommonService m
--   => SessionId
--   -> Text
--   -> [(Text, Maybe Text)]
--   -> m LB.ByteString
-- getCommon _    _           []  = throwError EmptyQueryArray
-- getCommon sess helpReqText arr = do
--   quan <- toQuantity helpReqText
--   case quan of
--     One -> do
--       help <- toHelpForRequest helpReqText
--       page <- getIntFromQueryArray arr "id_Entity"
--       getOneCommon sess help page
--     Plural -> do
--       help <- toHelpForRequest helpReqText
--       case help of
--         SortedNewsReq -> sortedNews arr
--         FilterNewsReq -> do
--           filteredNews arr
--         _ -> do
--           page <- getIntFromQueryArray arr "page"
--           getArrayCommon sess help page

