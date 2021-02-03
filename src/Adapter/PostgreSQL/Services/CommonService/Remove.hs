module Adapter.PostgreSQL.Services.CommonService.Remove where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude
    ( otherwise, ($), Eq((==)), Monad(return), Int ) 
import Control.Monad.Except ( MonadError(throwError) )
import Domain.Types.ImportTypes
  ( ErrorServer(DataErrorPostgreSQL, ErrorTakeEntityNotSupposed)
  , HelpForRequest(AuthorEntReq, CategoryEntReq, NewsEntReq, TagEntReq,
               UserEntReq)
  , LogLevel(Debug, ErrorLog)
  , errorText
  )
import Domain.Services.LogMonad (Log(writeLog))
import Database.PostgreSQL.Simple (execute)

remove :: PG r m => HelpForRequest -> Int -> m ()
remove ent idE
  | ent == AuthorEntReq = do
    let q = "DELETE FROM author WHERE id_author = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLog Debug "delete author good!"
        return ()
      _ -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | ent == UserEntReq = do
    let q = "DELETE FROM usernews WHERE id_user = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLog Debug "delete user good!"
        return ()
      _ -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | ent == NewsEntReq = do
    let q = "DELETE FROM news WHERE id_news = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLog Debug "delete news good!"
        return ()
      _ -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | ent == TagEntReq = do
    let q = "DELETE FROM tag WHERE id_tag = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLog Debug "delete tag good!"
        return ()
      _ -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | ent == CategoryEntReq = do
    let q = "DELETE FROM category WHERE id_category = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLog Debug "delete category1 good!"
        return ()
      _ -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | otherwise = do
    writeLog ErrorLog (errorText ErrorTakeEntityNotSupposed)
    throwError ErrorTakeEntityNotSupposed
