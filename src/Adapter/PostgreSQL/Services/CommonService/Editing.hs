module Adapter.PostgreSQL.Services.CommonService.Editing where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude ( ($), Monad(return), Maybe(Just, Nothing) )
import Domain.Types.ImportTypes
    ( errorText,
      ErrorServer(ErrorTakeEntityNotSupposed, DataErrorPostgreSQL),
      HelpForRequest(UserEntReq, AuthorEntReq, CategoryEntReq,
                     CommentEntReq, DraftEntReq, TagEntReq),
      UserId(userIdRaw),
      Category(parentCategory, nameCategory, idCategory),
      Draft(textDraft, dataCreateDraft, newsIdDraft, mainPhotoUrl,
            otherPhotoUrl, shortNameDraft, tagsId, idAuthorDraft, idDraft),
      Tag(nameTag, idTag),
      Author(idLinkUser, description, idAuthor),
      User(nameUser, lastName, userLogin, userPassword, avatar,
           dataCreate, userIsAdmin, userIsAuthor, idUser),
      Comment(textComments, dataCreateComments, newsIdComments,
              usersIdComments),
      AnEntity(..),
      Entity(getData, getHelpRequest) ) 
import Control.Monad.Except ( MonadError(throwError) )
import Database.PostgreSQL.Simple (execute)
import Domain.Services.LogMonad ( Log(writeLogE, writeLogD) ) 


editing :: PG r m => AnEntity -> m  ()
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
          writeLogD "update author good!"
          return ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    CategoryEntReq -> do
      let cat = (getData (AnEntity ent) :: Category)
      case parentCategory cat of
        Nothing  -> do
          let qMainCat = "UPDATE category SET name_category=(?) WHERE id_category=(?);"
          result <- withConn $ \conn -> execute conn qMainCat (nameCategory cat, idCategory cat)
          case result of
            1 -> do
              writeLogD "update main category good!"
              return ()
            _ -> do
              writeLogE (errorText DataErrorPostgreSQL)
              throwError DataErrorPostgreSQL
        Just pCat -> do
          let qNestedCat = "UPDATE category SET name_category=(?),parent_category =(?)  WHERE id_category=(?);"
          result <- withConn $ \conn -> execute conn qNestedCat  (nameCategory cat, idCategory pCat, cat)
          case result of
            1 -> do
              writeLogD "update nested category good!"
              editing (AnEntity pCat)
            _ -> do
              writeLogE (errorText DataErrorPostgreSQL)
              throwError DataErrorPostgreSQL
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
          writeLogD "update comment good!"
          return ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
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
          writeLogD "update draft good!"
          return ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    TagEntReq -> do
      let tag = (getData (AnEntity ent) :: Tag)
      let qTag = "UPDATE tag SET name_tag=(?) where id_tag= (?);"
      result <- withConn $ \conn -> execute conn qTag (nameTag tag, idTag tag)
      case result of
        1 -> do
          writeLogD "update tag good!"
          return  ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
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
          writeLogD "update user good!"
          return ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    _ -> do
      writeLogE (errorText ErrorTakeEntityNotSupposed)
      throwError ErrorTakeEntityNotSupposed
