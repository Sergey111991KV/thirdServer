module Adapter.PostgreSQL.Services.Auth
  ( findUserId
  , newSession
  , findUserIdBySession
  , deleteOldSession
  ) where

import Adapter.PostgreSQL.Common ( withConn, PG )
import ClassyPrelude
    ( ($), Monad(return), IO, MonadIO(liftIO), (++) )
import Database.PostgreSQL.Simple (execute, query)
import Domain.Services.LogMonad ( Log(writeLogE, writeLogD) )
import Domain.Types.ImportTypes
    ( errorText,
      ErrorServer(DataErrorPostgreSQL),
      Login,
      Password,
      SessionId,
      UserId(userIdRaw) )
import Text.StringRandom (stringRandomIO)
import Control.Monad.Except ( MonadError(throwError) )
  

findUserId :: PG r m => Login -> Password -> m UserId
findUserId login password = do
  let q =
        "SELECT id_user FROM userNews where login_user = (?) and password_user = (?)"
  i <- withConn $ \conn -> query conn q (login, password) :: IO [UserId]
  case i of
    [x] -> do
      writeLogD "findUserId success"
      return  x
    _ -> do
      writeLogE $ "Error findUserId " ++ errorText DataErrorPostgreSQL
      throwError DataErrorPostgreSQL

newSession :: PG r m => UserId -> m  SessionId
newSession user = do
  deleteOldSession user
  insertNewSession user
  let qry = "select key from session where user_news_id= ?"
  result <- withConn $ \conn -> query conn qry [userIdRaw user]
  case result of
        [sId] -> do
          writeLogD "Create New Session"
          return  sId
        _ -> do
          writeLogE "Error newSession"
          throwError DataErrorPostgreSQL

deleteOldSession :: PG r m => UserId -> m  ()
deleteOldSession us = do
  result <- withConn $ \conn -> execute conn qry [userIdRaw us]
  case result of
    1 -> do
      writeLogD "delete old session!"
      return  ()
    0 -> return ()  
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
 
  where
    qry = "delete from session where user_news_id = ?"

insertNewSession :: PG r m => UserId -> m  ()
insertNewSession uId = do
  sess <- liftIO $ stringRandomIO "[a-zA-Z0-9]{32}"
  result <- withConn $ \conn -> execute conn qry (sess, userIdRaw uId)
  case result of
    1 -> do
      writeLogD "insertNewSession!"
      return  ()
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
  where
    qry = "INSERT INTO session (key, user_news_id) values (?,?)"

findUserIdBySession :: PG r m => SessionId -> m  UserId
findUserIdBySession sesId = do
  result <- withConn $ \conn -> query conn qry sesId
  case result of
    [uIdStr] -> do
      writeLogD "findUserIdBySession good!"
      return  uIdStr
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
  where
    qry = "select user_news_id from session where key = ? "
