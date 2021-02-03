module Adapter.PostgreSQL.Services.AccessService where

import ClassyPrelude ( ($), Monad(return), Bool(..), IO )
import Adapter.PostgreSQL.Common ( withConn, PG )
import Database.PostgreSQL.Simple (Only(Only), query)
import Domain.Services.Auth (Auth(findUserIdBySession))

import Domain.Types.ImportTypes
    ( LogLevel(ErrorLog, Debug),
      ErrorServer(DataErrorPostgreSQL, NotAccessNotAdmid,
                  NotAccessNotAuthor),
      SessionId )
import Domain.Services.LogMonad (Log(writeLog))
import Control.Monad.Except ( MonadError(throwError) )
  
checkAdminAccess :: PG r m => SessionId -> m ()
checkAdminAccess sesId = do
    idU <- findUserIdBySession sesId
    resultAdmin <- withConn $ \conn -> query conn qry idU :: IO [Only Bool]
    case resultAdmin of
        [Only True] -> do
          writeLog Debug "checkAdminAccess True "
          return ()
        [Only False] -> do
          writeLog Debug "checkAdminAccess False "
          throwError NotAccessNotAdmid
        _ -> do
          writeLog ErrorLog "(errorText DataErrorPostgreSQL) "
          throwError DataErrorPostgreSQL
      where qry = "select admin from usernews where id_user = ? "



checkAuthorAccess :: PG r m => SessionId -> m ()
checkAuthorAccess sesId = do
    idA <- findUserIdBySession sesId
    resultAuthor <- withConn $ \conn -> query conn qry [idA] :: IO [Only Bool]
    case resultAuthor of
        [Only True] -> do
          writeLog Debug "checkAuthorAccess True "
          return ()
        [Only False] -> do
          writeLog Debug "checkAuthorAccess False "
          throwError NotAccessNotAuthor
        _ -> do
          writeLog ErrorLog "checkAuthorAccess DataErrorPostgreSQL "
          throwError DataErrorPostgreSQL
      where qry = "select authoris from usernews where id_user = ? "
