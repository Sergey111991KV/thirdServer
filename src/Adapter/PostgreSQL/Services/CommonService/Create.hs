module Adapter.PostgreSQL.Services.CommonService.Create where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude (Either(..), Maybe(Just, Nothing), Monad(return), ($))

import Domain.Types.ImportTypes
 

import Database.PostgreSQL.Simple (execute)
import Database.PostgreSQL.Simple.Types (Null(Null))
import Domain.Services.LogMonad (Log(writeLog))




create :: PG r m => AnEntity -> m (Either ErrorServer ())
create (AnEntity ent) = do
  case getHelpRequest  ent of
    AuthorEntReq -> do
      let author = (getData (AnEntity ent) :: Author)
      let q = "INSERT INTO author (id_link_user, description) VALUES (?,?);"
      result <-
        withConn $ \conn ->
          execute conn q (userIdRaw $ idLinkUser author, description author)
      case result of
        1 -> do
          writeLog Debug "create author good!"
          return $ Right ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    CategoryEntReq -> do
      let cat = (getData (AnEntity ent) :: Category)
      case parentCategory cat of
        Nothing  -> do
          let qMainCat = "INSERT INTO category (name_category,parent_category) VALUES (?,?);"
          result <- withConn $ \conn -> execute conn qMainCat (nameCategory cat, Database.PostgreSQL.Simple.Types.Null)
          case result of
            1 -> do
              writeLog Debug "create category good!"
              return $ Right ()
            _ -> do
              writeLog ErrorLog (errorText DataErrorPostgreSQL)
              return $ Left DataErrorPostgreSQL
        Just pCat -> do
          let qNestedCat = "INSERT INTO category (name_category,parent_category) VALUES (?,?);"
          result <- withConn $ \conn -> execute conn qNestedCat (nameCategory cat , idCategory pCat)
          case result of
            1 -> do
              writeLog Debug "create category good!"
              create (AnEntity pCat)
            _ -> do
              writeLog ErrorLog (errorText DataErrorPostgreSQL)
              return $ Left DataErrorPostgreSQL
    CommentEntReq -> do
      let comment = (getData (AnEntity ent) :: Comment)
      let q =
            "INSERT INTO comment (text_comment,data_create_comment,news_id_comment,user_id_comment) VALUES(?,?,?,?);"
      result <-
        withConn $ \conn ->
          execute
            conn
            q
            ( textComments comment
            , dataCreateComments comment
            , newsIdComments comment
            , usersIdComments comment)
      case result of
        1 -> do
          writeLog Debug "create comment good!"
          return $ Right ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    DraftEntReq  -> do
      let draft = (getData (AnEntity ent) :: Draft)
      let q =
            "INSERT INTO draft (text_draft, data_create_draft, news_id_draft, main_photo_draft,other_photo_draft, short_name_draft,tags_id, id_author_draft) VALUES (?,?,?,?,?,?,?,?);"
      result <-
        withConn $ \conn ->
          execute
            conn
            q
            ( textDraft draft
            , dataCreateDraft draft
            , newsIdDraft draft
            , mainPhotoUrl draft
            , otherPhotoUrl draft
            , shortNameDraft draft
            , tagsId draft
            , idAuthorDraft draft)
      case result of
        1 -> do
          writeLog Debug "create draft good!"
          return $ Right ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    TagEntReq -> do
      let tag = (getData (AnEntity ent) :: Tag)
      let qTag = "INSERT INTO tag (name_tag) VALUES(?);"
      result <- withConn $ \conn -> execute conn qTag [nameTag tag]
      case result of
        1 -> do
          writeLog Debug "create tag good!"
          return $ Right ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    UserEntReq -> do
      let user = (getData (AnEntity ent) :: User)
      let qUser =
            "INSERT INTO usernews (name_user, lastname , login_user , password_user , avatar_user , datacreate_user , admin , authoris)  VALUES (?,?,?,?,?,?,?,?);"
      result <-
        withConn $ \conn ->
          execute
            conn
            qUser
            ( nameUser user
            , lastName user
            , userLogin user
            , userPassword user
            , avatar user
            , dataCreate user
            , userIsAdmin user
            , userIsAuthor user)
      case result of
        1 -> do
          writeLog Debug "create user good!"
          return $ Right ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    _ -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL)
      return $ Left ErrorTakeEntityNotSupposed
