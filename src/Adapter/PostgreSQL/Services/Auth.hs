module Adapter.PostgreSQL.Services.Auth
  ( findUserId
  , newSession
  , findUserIdBySession
  , deleteOldSession
  ) where

import Adapter.PostgreSQL.Common
import ClassyPrelude

import Database.PostgreSQL.Simple (execute, query)
import Domain.Services.LogMonad (Log(writeLog))
import Domain.Types.ImportTypes
  ( ErrorServer(DataErrorPostgreSQL)
  , LogLevel(Debug, ErrorLog)
  , Login
  , Password
  , SessionId
  , UserId(userIdRaw)
  , errorText
  )
import Text.StringRandom (stringRandomIO)

findUserId :: PG r m => Login -> Password -> m (Either ErrorServer UserId)
findUserId login password = do
  let q =
        "SELECT id_user FROM userNews where login_user = (?) and password_user = (?)"
  i <- withConn $ \conn -> query conn q (login, password) :: IO [UserId]
  case i of
    [x] -> do
      writeLog Debug "findUserId success"
      return $ Right x
    [] -> do
      writeLog ErrorLog $ "Error findUserId " ++ errorText DataErrorPostgreSQL
      return $ Left DataErrorPostgreSQL

newSession :: PG r m => UserId -> m (Either ErrorServer SessionId)
newSession user = do
  resultDelete <- deleteOldSession user
  resultInsert <- insertNewSession user
  if resultDelete == Right () && resultInsert == Right ()
    then do
      let qry = "select key from session where user_news_id= ?"
      result <- withConn $ \conn -> query conn qry [userIdRaw user]
      case result of
        [sId] -> do
          writeLog Debug "Create New Session"
          return $ Right sId
        _ -> do
          writeLog ErrorLog "Error newSession"
          return $ Left DataErrorPostgreSQL
    else do
      return $ Left DataErrorPostgreSQL

deleteOldSession :: PG r m => UserId -> m (Either ErrorServer ())
deleteOldSession us = do
  result <- withConn $ \conn -> execute conn qry [userIdRaw us]
  case result of
    1 -> do
      writeLog Debug "delete old session!"
      return $ Right ()
    _ -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL)
      return $ Left DataErrorPostgreSQL
  where
    qry = "delete from session where user_news_id = ?"

insertNewSession :: PG r m => UserId -> m (Either ErrorServer ())
insertNewSession uId = do
  sess <- liftIO $ stringRandomIO "[a-zA-Z0-9]{32}"
  result <- withConn $ \conn -> execute conn qry (sess, userIdRaw uId)
  case result of
    1 -> do
      writeLog Debug "delete old session!"
      return $ Right ()
    _ -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL)
      return $ Left DataErrorPostgreSQL
  where
    qry = "INSERT INTO session (key, user_news_id) values (?,?)"

findUserIdBySession :: PG r m => SessionId -> m (Either ErrorServer UserId)
findUserIdBySession sesId = do
  result <- withConn $ \conn -> query conn qry sesId
  case result of
    [uIdStr] -> do
      writeLog Debug "findUserIdBySession good!"
      return $ Right uIdStr
    _ -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL)
      return $ Left DataErrorPostgreSQL
  where
    qry = "select user_news_id from session where key = ? "
