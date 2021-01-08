module Adapter.PostgreSQL.Services.CommonService.CheckAdminAccess where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude (Bool(..), Either(..), IO, Monad(return), ($))
import Database.PostgreSQL.Simple (Only(Only), query)
import Domain.Services.Auth (Auth(findUserIdBySession))
import Domain.Services.LogMonad (Log(writeLog))
import Domain.Types.ImportTypes
  ( ErrorServer(DataErrorPostgreSQL)
  , LogLevel(Debug, ErrorLog)
  , SessionId
  )

checkAdminAccess :: PG r m => SessionId -> m (Either ErrorServer Bool)
checkAdminAccess sesId = do
  resultSess <- findUserIdBySession sesId
  case resultSess of
    Left err -> return $ Left err
    Right idU -> do
      resultAdmin <- withConn $ \conn -> query conn qry idU :: IO [Only Bool]
      case resultAdmin of
        [Only True] -> do
          writeLog Debug "checkAdminAccess True "
          return $ Right True
        [Only False] -> do
          writeLog Debug "checkAdminAccess False "
          return $ Right False
        [] -> do
          writeLog ErrorLog "(errorText DataErrorPostgreSQL) "
          return $ Left DataErrorPostgreSQL
      where qry = "select admin from usernews where id_user = ? "
