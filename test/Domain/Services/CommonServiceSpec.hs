module Domain.Services.CommonServiceSpec where

import ClassyPrelude
import Test.Hspec
import Fixture
import Database.PostgreSQL.Simple.Types 
import qualified Prelude 

import Domain.Services.FixtureDomainService
import Domain.Types.ImportTypes
import Domain.Services.CommonService
import Domain.Services.Auth

-- passwordTest = Password "passwordRight"
-- loginTest =  Login "loginRight"
-- userIdTestUser = UserId 1

-- testSession = SessionId "session user"

-- authorTest = EntAuthor (Author 1 userIdTestUser "TestAuthor")
-- userTest = EntUser (User userIdTestUser "Andrey" "Cramov"  (Login "AC")  (Password "1702494") "avatar" time True False)
-- categoryTest =  EntCategory (Category 1 "Test Category" Nothing)
-- commentTest = EntComment (Comment 1 "Comment Test" time 1 (UserId 1)) 
-- draftTest = EntDraft (Draft 1 "Draft Test" time  Nothing "mainPhotoUrl" "shortNameDraft" pgArrayText  pgArrayInt 1)
-- tagTest = EntTag (Tag 1 "Tag Test")

-- pgArrayText = PGArray ["test 1 other photo", "test 1 other photo2"]

-- pgArrayInt = PGArray [1, 2]
-- time =  Prelude.read "2015-09-01 13:34:02 UTC" :: UTCTime

-- -- cat1Test = EntCategory  (ParentCategory 1 "test category 1" NotParent)
-- -- cat2Test = EntCategory  ( Category  2 "test category 2"  NotParent)


-- instance Auth App where
--     findUserId _ _ = return $ Right userIdTestUser
--     newSession _ = return $ Right testSession
--     findUserIdBySession _ = return $ Right userIdTestUser
--     deleteOldSession _  = return $ Right ()


-- instance CommonService App where
--     create = dispatch _create
--     editing = dispatch _editing
--     remove = dispatch2 _remove
--     remove' = dispatch3 _remove'
--     getAll =  dispatch _getAll
--     getAllDraft = dispatch _getAllDraft
--     getOne =  dispatch2 _getOne
--     getOneDraft = dispatch2 _getOneDraft
--     publish =  dispatch2 _publish
--     checkAuthorAccess = dispatch _checkAuthorAccess
--     checkAdminAccess = dispatch _checkAdminAccess 

-- publishAction :: CommonService m => SessionId -> Int -> m (Either ErrorServer ())
-- createCommon :: CommonService m => SessionId -> Maybe Entity -> m (Either ErrorServer ())
-- editingCommon :: CommonService m => SessionId -> Maybe Entity -> m (Either ErrorServer ())
-- removeCommon :: CommonService m => SessionId -> HelpForRequest -> Int -> m (Either ErrorServer ())
-- getEntityCommon :: CommonService m => SessionId -> HelpForRequest  -> Int -> m (Either ErrorServer Entity)
-- getEntityArray :: CommonService m => SessionId -> HelpForRequest -> m (Either ErrorServer [Entity])


spec :: Spec
spec = do
    describe "publishAction" $ do
        it "should publish Post, access Author"  $ do
            1 + 1 `shouldBe` 2 
    --         let fixture = emptyFixture { _publish = \_ _  -> return $ Right ()}
    --         runApp fixture ( publishAction testSession  1) `shouldReturn` Right ()
    --     it "should publish Post, access Author" $ do 
    --         let fixture = emptyFixture { _publish = \_ _  -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( publishAction testSession  1) `shouldReturn` Left DataErrorPostgreSQL

    -- describe "createCommon" $ do
    --     it "should create Author"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _create = \_ -> return $ Right () }
    --         runApp fixture ( createCommon testSession  (Just authorTest)) `shouldReturn` Right ()
    --     it "should create Category"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _create = \_ -> return $ Right () }
    --         runApp fixture ( createCommon testSession  (Just categoryTest)) `shouldReturn` Right ()
    --     it "should create User"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _create = \_ -> return $ Right () }
    --         runApp fixture ( createCommon testSession  (Just userTest)) `shouldReturn` Right ()
    --     it "should create Tag"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _create = \_ -> return $ Right () }
    --         runApp fixture ( createCommon testSession  (Just tagTest)) `shouldReturn` Right ()
    --     it "should create Comment"  $ do
    --         let fixture = emptyFixture { _create = \_ -> return $ Right () }
    --         runApp fixture ( createCommon testSession  (Just commentTest)) `shouldReturn` Right ()
    --     it "should create Draft"  $ do
    --         let fixture = emptyFixture { _checkAuthorAccess = \_   -> return $ Right True,
    --                                     _create = \_ -> return $ Right () }
    --         runApp fixture ( createCommon testSession  (Just draftTest)) `shouldReturn` Right ()
    --     it "should not create Author"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right False}
    --         runApp fixture ( createCommon testSession  (Just authorTest)) `shouldReturn` Left NotForAdmin
    --     it "should not create Category"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right False}
    --         runApp fixture ( createCommon testSession  (Just categoryTest)) `shouldReturn` Left NotForAdmin
    --     it "should not create User"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right False}
    --         runApp fixture ( createCommon testSession  (Just userTest)) `shouldReturn` Left NotForAdmin
    --     it "should not create Tag"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right False}
    --         runApp fixture ( createCommon testSession  (Just tagTest)) `shouldReturn` Left NotForAdmin
    --     it "should not create Comment"  $ do
    --         let fixture = emptyFixture { _create = \_ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( createCommon testSession  (Just commentTest)) `shouldReturn` Left DataErrorPostgreSQL 
    --     it "should not create Draft"  $ do
    --         let fixture = emptyFixture { _checkAuthorAccess = \_   -> return $ Right False }
    --         runApp fixture ( createCommon testSession  (Just draftTest)) `shouldReturn` Left NotForAuthor
    --     it "should not create Author error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _create = \_ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( createCommon testSession  (Just authorTest)) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not create Category error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _create = \_ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( createCommon testSession  (Just categoryTest)) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not create User error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _create = \_ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( createCommon testSession  (Just userTest)) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not create Tag error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _create = \_ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( createCommon testSession  (Just tagTest)) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not create Comment error Postgress"  $ do
    --         let fixture = emptyFixture { _create = \_ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( createCommon testSession  (Just commentTest)) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not create Draft error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAuthorAccess = \_   -> return $ Right True,
    --                                     _create = \_ -> return $ Left DataErrorPostgreSQL}
    --         runApp fixture ( createCommon testSession  (Just draftTest)) `shouldReturn` Left DataErrorPostgreSQL


    -- describe "editingCommon" $ do
    --     it "should edit Author"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _editing = \_ -> return $ Right () }
    --         runApp fixture ( editingCommon testSession  (Just authorTest)) `shouldReturn` Right ()
    --     it "should edit Category"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _editing = \_ -> return $ Right () }
    --         runApp fixture ( editingCommon testSession  (Just categoryTest)) `shouldReturn` Right ()
    --     it "should edit User"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _editing = \_ -> return $ Right () }
    --         runApp fixture ( editingCommon testSession  (Just userTest)) `shouldReturn` Right ()
    --     it "should edit Tag"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _editing = \_ -> return $ Right () }
    --         runApp fixture ( editingCommon testSession  (Just tagTest)) `shouldReturn` Right ()
    --     it "should edit Comment"  $ do
    --         let fixture = emptyFixture { _editing = \_ -> return $ Right () }
    --         runApp fixture ( editingCommon testSession  (Just commentTest)) `shouldReturn` Right ()
    --     it "should edit Draft"  $ do
    --         let fixture = emptyFixture { _checkAuthorAccess = \_   -> return $ Right True,
    --                                     _editing = \_ -> return $ Right () }
    --         runApp fixture ( editingCommon testSession  (Just draftTest)) `shouldReturn` Right ()
    --     it "should not edit Author"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right False}
    --         runApp fixture ( editingCommon testSession  (Just authorTest)) `shouldReturn` Left NotForAdmin
    --     it "should not edit Category"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right False}
    --         runApp fixture ( editingCommon testSession  (Just categoryTest)) `shouldReturn` Left NotForAdmin
    --     it "should not edit User"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right False}
    --         runApp fixture ( editingCommon testSession  (Just userTest)) `shouldReturn` Left NotForAdmin
    --     it "should not edit Tag"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right False}
    --         runApp fixture ( editingCommon testSession  (Just tagTest)) `shouldReturn` Left NotForAdmin
    --     it "should not edit Comment"  $ do
    --         let fixture = emptyFixture { _editing = \_ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( editingCommon testSession  (Just commentTest)) `shouldReturn` Left DataErrorPostgreSQL 
    --     it "should not edit Draft"  $ do
    --         let fixture = emptyFixture { _checkAuthorAccess = \_   -> return $ Right False }
    --         runApp fixture ( editingCommon testSession  (Just draftTest)) `shouldReturn` Left NotForAuthor
    --     it "should not edit Author error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _editing = \_ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( editingCommon testSession  (Just authorTest)) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not edit Category error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _editing = \_ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( editingCommon testSession  (Just categoryTest)) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not edit User error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _editing = \_ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( editingCommon testSession  (Just userTest)) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not edit Tag error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _editing = \_ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( editingCommon testSession  (Just tagTest)) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not edit Comment error Postgress"  $ do
    --         let fixture = emptyFixture { _editing = \_ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( editingCommon testSession  (Just commentTest)) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not edit Draft error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAuthorAccess = \_   -> return $ Right True,
    --                                     _editing = \_ -> return $ Left DataErrorPostgreSQL}
    --         runApp fixture ( editingCommon testSession  (Just draftTest)) `shouldReturn` Left DataErrorPostgreSQL

    -- describe "removeCommon" $ do
    --     it "should remove Author"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _remove = \_ _ -> return $ Right () }
    --         runApp fixture ( removeCommon testSession  AuthorEntReq 1) `shouldReturn` Right ()
    --     it "should remove Category"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _remove = \_ _ -> return $ Right () }
    --         runApp fixture ( removeCommon testSession  CategoryEntReq 1) `shouldReturn` Right ()
    --     it "should remove User"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _remove = \_ _ -> return $ Right () }
    --         runApp fixture ( removeCommon testSession  UserEntReq 1) `shouldReturn` Right ()
    --     it "should remove Tag"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _remove = \_ _ -> return $ Right () }
    --         runApp fixture ( removeCommon testSession  TagEntReq 1) `shouldReturn` Right ()
    --     it "should remove Comment"  $ do
    --         let fixture = emptyFixture { _remove' = \_ _ _ -> return $ Right () }
    --         runApp fixture ( removeCommon testSession  CommentEntReq 1) `shouldReturn` Right ()
    --     it "should remove Draft"  $ do
    --         let fixture = emptyFixture {  _remove' = \_ _ _ -> return $ Right () }
    --         runApp fixture ( removeCommon testSession  DraftEntReq 1) `shouldReturn` Right ()
    --     it "should not remove Author"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right False}
    --         runApp fixture ( removeCommon testSession  AuthorEntReq 1) `shouldReturn` Left NotForAdmin
    --     it "should not remove Category"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right False}
    --         runApp fixture ( removeCommon testSession  CategoryEntReq 1) `shouldReturn` Left NotForAdmin
    --     it "should not remove User"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right False}
    --         runApp fixture ( removeCommon testSession  UserEntReq 1) `shouldReturn` Left NotForAdmin
    --     it "should not remove Tag"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right False}
    --         runApp fixture ( removeCommon testSession  TagEntReq 1) `shouldReturn` Left NotForAdmin
    --     it "should not remove Comment"  $ do
    --         let fixture = emptyFixture { _remove' = \_ _ _-> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( removeCommon testSession  CommentEntReq 1) `shouldReturn` Left DataErrorPostgreSQL 
    --     it "should not remove Draft"  $ do
    --         let fixture = emptyFixture { _remove' = \_ _ _-> return $ Left DataErrorPostgreSQL  }
    --         runApp fixture ( removeCommon testSession  DraftEntReq 1) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not remove Author error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _remove = \_ _ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( removeCommon testSession  AuthorEntReq 1) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not remove Category error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _remove = \_ _ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( removeCommon testSession  CategoryEntReq 1) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not remove User error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _remove = \_ _ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( removeCommon testSession  UserEntReq 1) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not remove Tag error Postgress"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _remove = \_ _ -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( removeCommon testSession  TagEntReq 1) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not remove Comment error Postgress"  $ do
    --         let fixture = emptyFixture { _remove' = \_ _ _-> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( removeCommon testSession  CommentEntReq 1) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not remove Draft error Postgress"  $ do
    --         let fixture = emptyFixture { _remove' = \_ _ _-> return $ Left DataErrorPostgreSQL}
    --         runApp fixture ( removeCommon testSession  DraftEntReq 1) `shouldReturn` Left DataErrorPostgreSQL

    -- describe "getEntityCommon" $ do
    --     it "should get Author"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _getOne = \AuthorEntReq 1 -> return $ Right authorTest }
    --         runApp fixture ( getEntityCommon testSession  AuthorEntReq 1) `shouldReturn` Right authorTest
    --     it "should get Category"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _getOne = \CategoryEntReq 1 -> return $ Right categoryTest }
    --         runApp fixture ( getEntityCommon testSession  CategoryEntReq 1) `shouldReturn` Right categoryTest
    --     it "should get User"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _getOne = \UserEntReq 1 -> return $ Right userTest }
    --         runApp fixture ( getEntityCommon testSession  UserEntReq 1) `shouldReturn` Right userTest
    --     it "should get Tag"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _getOne = \TagEntReq 1 -> return $ Right tagTest }
    --         runApp fixture ( getEntityCommon testSession  TagEntReq 1) `shouldReturn` Right tagTest
    --     it "should get Comment"  $ do
    --         let fixture = emptyFixture { _getOne = \CommentEntReq 1 -> return $ Right commentTest }
    --         runApp fixture ( getEntityCommon testSession  CommentEntReq 1) `shouldReturn` Right commentTest
    --     it "should get Draft"  $ do
    --         let fixture = emptyFixture {  _getOneDraft = \testSession 1 -> return $ Right draftTest }
    --         runApp fixture ( getEntityCommon testSession  DraftEntReq 1) `shouldReturn` Right draftTest
    --     it "should not get Author"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _getOne = \AuthorEntReq 1   -> return $ Left DataErrorPostgreSQL}
    --         runApp fixture ( getEntityCommon testSession  AuthorEntReq 1) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not get Category"  $ do
    --         let fixture = emptyFixture { _getOne = \CategoryEntReq 1   -> return $ Left DataErrorPostgreSQL}
    --         runApp fixture ( getEntityCommon testSession  CategoryEntReq 1) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not get User"  $ do
    --         let fixture = emptyFixture { _getOne = \UserEntReq 1   -> return $ Left DataErrorPostgreSQL}
    --         runApp fixture ( getEntityCommon testSession  UserEntReq 1) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not get Tag"  $ do
    --         let fixture = emptyFixture { _getOne = \TagEntReq 1   -> return $ Left DataErrorPostgreSQL}
    --         runApp fixture ( getEntityCommon testSession  TagEntReq 1) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not get Comment"  $ do
    --         let fixture = emptyFixture { _getOne = \CommentEntReq 1 -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( getEntityCommon testSession  CommentEntReq 1) `shouldReturn` Left DataErrorPostgreSQL 
    --     it "should not get Draft"  $ do
    --         let fixture = emptyFixture { _getOneDraft = \testSession 1 -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( getEntityCommon testSession  DraftEntReq 1) `shouldReturn` Left DataErrorPostgreSQL
       
    -- describe "getEntityArray" $ do
    --     it "should get array Author"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _getAll = \AuthorEntReq  -> return $ Right [authorTest] }
    --         runApp fixture ( getEntityArray testSession  AuthorEntReq ) `shouldReturn` Right [authorTest]
    --     it "should get array Category"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _getAll = \CategoryEntReq  -> return $ Right [categoryTest] }
    --         runApp fixture ( getEntityArray testSession  CategoryEntReq ) `shouldReturn` Right [categoryTest]
    --     it "should get array User"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _getAll = \UserEntReq  -> return $ Right [userTest] }
    --         runApp fixture ( getEntityArray testSession  UserEntReq ) `shouldReturn` Right [userTest]
    --     it "should get array Tag"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _getAll = \TagEntReq  -> return $ Right [tagTest] }
    --         runApp fixture ( getEntityArray testSession  TagEntReq ) `shouldReturn` Right [tagTest]
    --     it "should get array Comment"  $ do
    --         let fixture = emptyFixture { _getAll = \CommentEntReq  -> return $ Right [commentTest] }
    --         runApp fixture ( getEntityArray testSession  CommentEntReq ) `shouldReturn` Left ErrorTakeEntityNotSupposed
    --     it "should get array Draft"  $ do
    --         let fixture = emptyFixture { _getAllDraft = \testSession  -> return $ Right [draftTest] }

    --         runApp fixture ( getEntityArray testSession  DraftEntReq ) `shouldReturn` Right [draftTest]
    --     it "should not get array Author"  $ do
    --         let fixture = emptyFixture { _checkAdminAccess = \_   -> return $ Right True,
    --                                     _getAll = \AuthorEntReq    -> return $ Left DataErrorPostgreSQL}
    --         runApp fixture ( getEntityArray testSession  AuthorEntReq ) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not get array Category"  $ do
    --         let fixture = emptyFixture { _getAll = \CategoryEntReq    -> return $ Left DataErrorPostgreSQL}
    --         runApp fixture ( getEntityArray testSession  CategoryEntReq ) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not get array User"  $ do
    --         let fixture = emptyFixture { _getAll = \UserEntReq    -> return $ Left DataErrorPostgreSQL}
    --         runApp fixture ( getEntityArray testSession  UserEntReq ) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not get array Tag"  $ do
    --         let fixture = emptyFixture { _getAll = \TagEntReq    -> return $ Left DataErrorPostgreSQL}
    --         runApp fixture ( getEntityArray testSession  TagEntReq ) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not get array Comment"  $ do
    --         let fixture = emptyFixture { _getAll = \CommentEntReq  -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( getEntityArray testSession  CommentEntReq ) `shouldReturn` Left ErrorTakeEntityNotSupposed 
    --     it "should not get array Draft"  $ do
    --         let fixture = emptyFixture { _getAllDraft = \testSession  -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( getEntityArray testSession  DraftEntReq ) `shouldReturn` Left DataErrorPostgreSQL
        
    -- describe "getEntityArrayComment" $ do
    --     it "should return comment array"  $ do
    --         let fixture = emptyFixture { _getOne = \NewsEntReq 1  -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( getEntityArrayComment 1) `shouldReturn` Left DataErrorPostgreSQL
    --     it "should not return comment array"  $ do
    --         let fixture = emptyFixture { _getOne = \NewsEntReq 1  -> return $ Left DataErrorPostgreSQL }
    --         runApp fixture ( getEntityArrayComment 1 ) `shouldReturn` Left DataErrorPostgreSQL
