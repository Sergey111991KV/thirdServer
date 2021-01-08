module Adapter.PostgreSQL.Services.CommonService.CheckAuthorAccess where

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

checkAuthorAccess :: PG r m => SessionId -> m (Either ErrorServer Bool)
checkAuthorAccess sesId = do
  resultSess <- findUserIdBySession sesId
  case resultSess of
    Left err -> return $ Left err
    Right idA -> do
      resultAuthor <- withConn $ \conn -> query conn qry [idA] :: IO [Only Bool]
      case resultAuthor of
        [Only True] -> do
          writeLog Debug "checkAuthorAccess True "
          return $ Right True
        [Only False] -> do
          writeLog Debug "checkAuthorAccess False "
          return $ Right False
        [] -> do
          writeLog ErrorLog "checkAuthorAccess DataErrorPostgreSQL "
          return $ Left DataErrorPostgreSQL
      where qry = "select authoris from usernews where id_user = ? "
