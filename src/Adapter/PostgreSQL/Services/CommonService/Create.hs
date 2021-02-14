module Adapter.PostgreSQL.Services.CommonService.Create where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude ( ($), Monad(return), Maybe(Just, Nothing) ) 
import Domain.Types.ExportTypes
    ( errorText,
      ErrorServer(ErrorTakeEntityNotSupposed, DataErrorPostgreSQL),
      HelpForRequest(UserEntReq, AuthorEntReq, CategoryEntReq,
                     CommentEntReq, DraftEntReq, TagEntReq),
      UserId(userIdRaw),
      Category(parentCategory, nameCategory, idCategory),
      Draft(textDraft, dataCreateDraft, newsIdDraft, mainPhotoUrl,
            otherPhotoUrl, shortNameDraft, tagsId, idAuthorDraft),
      Tag(nameTag),
      Author(idLinkUser, description),
      User(nameUser, lastName, userLogin, userPassword, avatar,
           dataCreate, userIsAdmin, userIsAuthor),
      Comment(textComments, dataCreateComments, newsIdComments,
              usersIdComments),
      AnEntity(..),
      Entity(getData, getHelpRequest) )
import Control.Monad.Except ( MonadError(throwError) )
import Database.PostgreSQL.Simple (execute)
import Database.PostgreSQL.Simple.Types (Null(Null))
import Domain.Services.LogMonad ( Log(writeLogE, writeLogD) ) 



create :: PG r m => AnEntity -> m  ()
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
          writeLogD "create author good!"
          return ()
        _ -> do
          writeLogE  (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    CategoryEntReq -> do
      let cat = (getData (AnEntity ent) :: Category)
      case parentCategory cat of
        Nothing  -> do
          let qMainCat = "INSERT INTO category (name_category,parent_category) VALUES (?,?);"
          result <- withConn $ \conn -> execute conn qMainCat (nameCategory cat, Database.PostgreSQL.Simple.Types.Null)
          case result of
            1 -> do
              writeLogD "create category good!"
              return ()
            _ -> do
              writeLogE (errorText DataErrorPostgreSQL)
              throwError DataErrorPostgreSQL
        Just pCat -> do
          let qNestedCat = "INSERT INTO category (name_category,parent_category) VALUES (?,?);"
          result <- withConn $ \conn -> execute conn qNestedCat (nameCategory cat , idCategory pCat)
          case result of
            1 -> do
              writeLogD "create category good!"
              create (AnEntity pCat)
            _ -> do
              writeLogE (errorText DataErrorPostgreSQL)
              throwError DataErrorPostgreSQL
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
          writeLogD "create comment good!"
          return ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
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
          writeLogD "create draft good!"
          return  ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    TagEntReq -> do
      let tag = (getData (AnEntity ent) :: Tag)
      let qTag = "INSERT INTO tag (name_tag) VALUES(?);"
      result <- withConn $ \conn -> execute conn qTag [nameTag tag]
      case result of
        1 -> do
          writeLogD "create tag good!"
          return  ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
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
          writeLogD "create user good!"
          return  ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError ErrorTakeEntityNotSupposed
