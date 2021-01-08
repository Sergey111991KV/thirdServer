module Adapter.PostgreSQL.Services.CommonService.Remove where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude (Either(..), Eq((==)), Int, Monad(return), ($), otherwise)

import Domain.Types.ImportTypes
  ( ErrorServer(DataErrorPostgreSQL, ErrorTakeEntityNotSupposed)
  , HelpForRequest(AuthorEntReq, CategoryEntReq, NewsEntReq, TagEntReq,
               UserEntReq)
  , LogLevel(Debug, ErrorLog)
  , errorText
  )

import Domain.Services.LogMonad (Log(writeLog))

import Database.PostgreSQL.Simple (execute)

remove :: PG r m => HelpForRequest -> Int -> m (Either ErrorServer ())
remove ent idE
  | ent == AuthorEntReq = do
    let q = "DELETE FROM author WHERE id_author = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLog Debug "delete author good!"
        return $ Right ()
      _ -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        return $ Left DataErrorPostgreSQL
  | ent == UserEntReq = do
    let q = "DELETE FROM usernews WHERE id_user = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLog Debug "delete user good!"
        return $ Right ()
      _ -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        return $ Left DataErrorPostgreSQL
  | ent == NewsEntReq = do
    let q = "DELETE FROM news WHERE id_news = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLog Debug "delete news good!"
        return $ Right ()
      _ -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        return $ Left DataErrorPostgreSQL
  | ent == TagEntReq = do
    let q = "DELETE FROM tag WHERE id_tag = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLog Debug "delete tag good!"
        return $ Right ()
      _ -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        return $ Left DataErrorPostgreSQL
  | ent == CategoryEntReq = do
    let q = "DELETE FROM category WHERE id_category = (?);"
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLog Debug "delete category1 good!"
        return $ Right ()
      _ -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        return $ Left DataErrorPostgreSQL
  | otherwise = do
    writeLog ErrorLog (errorText ErrorTakeEntityNotSupposed)
    return $ Left ErrorTakeEntityNotSupposed
