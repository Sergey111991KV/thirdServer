module Adapter.PostgreSQL.Services.CommonService.Editing where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude (Either(..), Maybe(Just, Nothing), Monad(return), ($))

import Domain.Types.ImportTypes
  

import Database.PostgreSQL.Simple (execute)
import Domain.Services.LogMonad (Log(writeLog))


editing :: PG r m => AnEntity -> m (Either ErrorServer ())
editing (AnEntity ent) = do
  case getHelpRequest  ent of
    AuthorEntReq -> do
      let author = (getData (AnEntity ent) :: Author)
      let q = "UPDATE author SET id_link_user=(?), description=(?) WHERE id_author = (?) ;"
      result <-
        withConn $ \conn ->
          execute
            conn
            q
            (userIdRaw $ idLinkUser author, description author, idAuthor author)
      case result of
        1 -> do
          writeLog Debug "update author good!"
          return $ Right ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    CategoryEntReq -> do
      let cat = (getData (AnEntity ent) :: Category)
      case parentCategory cat of
        Nothing  -> do
          let qMainCat = "UPDATE category SET name_category=(?) WHERE id_category=(?);"
          result <- withConn $ \conn -> execute conn qMainCat (nameCategory cat, idCategory cat)
          case result of
            1 -> do
              writeLog Debug "update main category good!"
              return $ Right ()
            _ -> do
              writeLog ErrorLog (errorText DataErrorPostgreSQL)
              return $ Left DataErrorPostgreSQL
        Just pCat -> do
          let qNestedCat = "UPDATE category SET name_category=(?),parent_category =(?)  WHERE id_category=(?);"
          result <- withConn $ \conn -> execute conn qNestedCat  (nameCategory cat, idCategory pCat, cat)
          case result of
            1 -> do
              writeLog Debug "update nested category good!"
              editing (AnEntity pCat)
            _ -> do
              writeLog ErrorLog (errorText DataErrorPostgreSQL)
              return $ Left DataErrorPostgreSQL
    CommentEntReq -> do
      let comment = (getData (AnEntity ent) :: Comment)
      let q =
            "UPDATE comment SET (text_comment,data_create_comment,news_id_comment,user_id_comment) VALUES(?,?,?,?);"
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
          writeLog Debug "update comment good!"
          return $ Right ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    DraftEntReq  -> do
      let draft = (getData (AnEntity ent) :: Draft)
      let q =
            "UPDATE draft SET text_draft=(?), data_create_draft=(?), news_id_draft=(?), main_photo_draft=(?), other_photo_draft=(?), short_name_draft=(?), tags_id=(?), id_author_draft=(?) where id_draft =(?);"
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
            , idAuthorDraft draft
            , idDraft draft)
      case result of
        1 -> do
          writeLog Debug "update draft good!"
          return $ Right ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    TagEntReq -> do
      let tag = (getData (AnEntity ent) :: Tag)
      let qTag = "UPDATE tag SET name_tag=(?) where id_tag= (?);"
      result <- withConn $ \conn -> execute conn qTag (nameTag tag, idTag tag)
      case result of
        1 -> do
          writeLog Debug "update tag good!"
          return $ Right ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    UserEntReq -> do
      let user = (getData (AnEntity ent) :: User)
      let qUser =
            "UPDATE usernews SET name_user=(?), lastname=(?) , login_user=(?) , password_user=(?) , avatar_user=(?) , datacreate_user=(?) , admin=(?) , authoris=(?)  where id_user=(?);"
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
            , userIsAuthor user
            , idUser user)
      case result of
        1 -> do
          writeLog Debug "update user good!"
          return $ Right ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    _ -> do
      writeLog ErrorLog (errorText ErrorTakeEntityNotSupposed)
      return $ Left ErrorTakeEntityNotSupposed
