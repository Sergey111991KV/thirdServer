module Adapter.PostgreSQL.Services.CommonService.Remove where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude
    ( otherwise, ($), Eq((==)), Monad(return), Int ) 
import Control.Monad.Except ( MonadError(throwError) )
import Domain.Types.ImportTypes
    ( errorText,
      ErrorServer(ErrorTakeEntityNotSupposed, DataErrorPostgreSQL),
      HelpForRequest(CategoryEntReq, AuthorEntReq, UserEntReq,
                     NewsEntReq, TagEntReq) )
import Domain.Services.LogMonad ( Log(writeLogE, writeLogD) ) 
import Database.PostgreSQL.Simple (execute)

remove :: PG r m => HelpForRequest -> Int -> m ()
remove ent idE
  | ent == AuthorEntReq = do
    let q = "DELETE FROM author WHERE id_author = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLogD "delete author good!"
        return ()
      _ -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | ent == UserEntReq = do
    let q = "DELETE FROM usernews WHERE id_user = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLogD "delete user good!"
        return ()
      _ -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | ent == NewsEntReq = do
    let q = "DELETE FROM news WHERE id_news = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLogD "delete news good!"
        return ()
      _ -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | ent == TagEntReq = do
    let q = "DELETE FROM tag WHERE id_tag = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLogD "delete tag good!"
        return ()
      _ -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | ent == CategoryEntReq = do
    let q = "DELETE FROM category WHERE id_category = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLogD "delete category1 good!"
        return ()
      _ -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | otherwise = do
    writeLogE (errorText ErrorTakeEntityNotSupposed)
    throwError ErrorTakeEntityNotSupposed
