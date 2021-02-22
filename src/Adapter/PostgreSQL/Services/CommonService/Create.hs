{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.CommonService.Create where

import Adapter.PostgreSQL.Common ( withConn, PG )
import ClassyPrelude ( ($), Monad(return), Maybe(Just, Nothing) )
import Domain.Types.ExportTypes
    ( errorText,
      ErrorServer(ErrorTakeEntityNotSupposed, DataErrorPostgreSQL),
      UserId(userIdRaw),
      Draft(textDraft, dataCreateDraft, newsIdDraft, mainPhotoUrl,
            otherPhotoUrl, shortNameDraft, tagsId, idAuthorDraft),
      Category(parentCategory, nameCategory, idCategory),
      Tag(nameTag),
      Comment(textComments, dataCreateComments, newsIdComments,
              usersIdComments),
      Author(idLinkUser, description),
      User(nameUser, lastName, userLogin, userPassword, avatar,
           dataCreate, userIsAdmin, userIsAuthor),
      AnEntity(AnUser, AnAuthor, AnCategory, AnComment, AnDraft, AnTag) )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Database.PostgreSQL.Simple.Types
                                                ( Null(Null) )
import           Domain.Services.LogMonad       ( Log(writeLogE, writeLogD) )
import Adapter.PostgreSQL.ImportLibrary ( execute, sql )



create :: PG r m => AnEntity -> m ()
create (AnAuthor author) = do
  result <- withConn $ \conn -> execute
    conn
    [sql| INSERT INTO author (id_link_user, description) VALUES (?,?);|]
    (userIdRaw $ idLinkUser author, description author)
  case result of
    1 -> do
      writeLogD "create author good!"
      return ()
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
create (AnCategory cat) = do
  case parentCategory cat of
    Nothing -> do
      result <- withConn $ \conn -> execute
        conn
        [sql| INSERT INTO category (name_category,parent_category) VALUES (?,?);|]
        (nameCategory cat, Database.PostgreSQL.Simple.Types.Null)
      case result of
        1 -> do
          writeLogD "create category good!"
          return ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    Just pCat -> do
      result <- withConn $ \conn -> execute
        conn
        [sql| INSERT INTO category (name_category,parent_category) VALUES (?,?);|]
        (nameCategory cat, idCategory pCat)
      case result of
        1 -> do
          writeLogD "create category good!"
          create (AnCategory pCat)
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
create (AnComment comment) = do
  result <- withConn $ \conn -> execute
    conn
    [sql| INSERT INTO comment (text_comment,data_create_comment,
                                      news_id_comment,user_id_comment) 
                                      VALUES(?,?,?,?);
                |]
    ( textComments comment
    , dataCreateComments comment
    , newsIdComments comment
    , usersIdComments comment
    )
  case result of
    1 -> do
      writeLogD "create comment good!"
      return ()
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
create (AnDraft draft) = do
  result <- withConn $ \conn -> execute
    conn
    [sql| INSERT INTO draft (text_draft, data_create_draft, 
                                    news_id_draft, main_photo_draft,other_photo_draft, 
                                    short_name_draft,tags_id, id_author_draft) 
                                    VALUES (?,?,?,?,?,?,?,?);
                |]
    ( textDraft draft
    , dataCreateDraft draft
    , newsIdDraft draft
    , mainPhotoUrl draft
    , otherPhotoUrl draft
    , shortNameDraft draft
    , tagsId draft
    , idAuthorDraft draft
    )
  case result of
    1 -> do
      writeLogD "create draft good!"
      return ()
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
create (AnTag tag) = do
  result <- withConn $ \conn ->
    execute conn [sql| INSERT INTO tag (name_tag) VALUES(?); |] [nameTag tag]
  case result of
    1 -> do
      writeLogD "create tag good!"
      return ()
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
create (AnUser user) = do
  result <- withConn $ \conn -> execute
    conn
    [sql| INSERT INTO usernews 
                (name_user, lastname , 
                login_user , password_user , 
                avatar_user , datacreate_user , 
                admin , authoris)  VALUES 
                (?,?,?,?,?,?,?,?);   |]
    ( nameUser user
    , lastName user
    , userLogin user
    , userPassword user
    , avatar user
    , dataCreate user
    , userIsAdmin user
    , userIsAuthor user
    )
  case result of
    1 -> do
      writeLogD "create user good!"
      return ()
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
create _ = do
  writeLogE (errorText DataErrorPostgreSQL)
  throwError ErrorTakeEntityNotSupposed
