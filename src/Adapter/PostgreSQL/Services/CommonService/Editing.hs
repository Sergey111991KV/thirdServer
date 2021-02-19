{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.CommonService.Editing where

import           Adapter.PostgreSQL.Common      ( PG
                                                , withConn
                                                )
import           ClassyPrelude
import           Domain.Types.ExportTypes
import           Adapter.PostgreSQL.ImportLibrary
import           Control.Monad.Except           ( MonadError(throwError) )
import           Domain.Services.LogMonad       ( Log(writeLogE, writeLogD) )


editing :: PG r m => AnEntity -> m ()
editing (AnAuthor author) = do
  let
    q
      = [sql| UPDATE author SET id_link_user=(?), description=(?) WHERE id_author = (?);|]
  result <- withConn $ \conn -> execute
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
editing (AnCategory cat) = do
  case parentCategory cat of
    Nothing -> do
      let qMainCat =
            [sql| UPDATE category SET name_category=(?) WHERE id_category=(?);|]
      result <- withConn
        $ \conn -> execute conn qMainCat (nameCategory cat, idCategory cat)
      liftIO $ print result
      case result of
        1 -> do
          writeLogD "update main category good!"
          return ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    Just pCat -> do
      let
        qNestedCat
          = [sql| UPDATE category SET name_category=(?),parent_category =(?)  WHERE id_category=(?);|]
      result <- withConn $ \conn -> execute
        conn
        qNestedCat
        (nameCategory cat, idCategory pCat, idCategory cat)
      liftIO $ print result
      case result of
        1 -> do
          writeLogD "update nested category good!"
          editing (AnCategory pCat)
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
editing (AnComment comment) = do
  let
    q
      = [sql| UPDATE comment SET (text_comment,data_create_comment,news_id_comment,user_id_comment) VALUES(?,?,?,?);|]
  result <- withConn $ \conn -> execute
    conn
    q
    ( textComments comment
    , dataCreateComments comment
    , newsIdComments comment
    , usersIdComments comment
    )
  case result of
    1 -> do
      writeLogD "update comment good!"
      return ()
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
editing (AnTag tag) = do
  let qTag = [sql| UPDATE tag SET name_tag=(?) where id_tag= (?);|]
  result <- withConn $ \conn -> execute conn qTag (nameTag tag, idTag tag)
  case result of
    1 -> do
      writeLogD "update tag good!"
      return ()
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
editing (AnUser user) = do
  let
    qUser
      = [sql| UPDATE usernews SET name_user=(?), lastname=(?) , login_user=(?) , password_user=(?) , avatar_user=(?) , datacreate_user=(?) , admin=(?) , authoris=(?)  where id_user=(?);|]
  result <- withConn $ \conn -> execute
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
    , idUser user
    )
  case result of
    1 -> do
      writeLogD "update user good!"
      return ()
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
editing _ = do
  writeLogE (errorText DataErrorPostgreSQL)
  throwError ErrorTakeEntityNotSupposed
