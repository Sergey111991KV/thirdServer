{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.AccessService where

import           ClassyPrelude                  ( ($)
                                                , Monad(return)
                                                , Bool(..)
                                                , Int
                                                , IO
                                                )

import           Adapter.PostgreSQL.Common      ( withConn
                                                , PG
                                                )
import           Domain.Services.Auth           ( Auth(findUserIdBySession) )
import           Domain.Types.ExportTypes       ( ErrorServer
                                                  ( DataErrorPostgreSQL
                                                  , NotAccessNotAdmid
                                                  , NotAccessNotAuthor
                                                  )
                                                , SessionId
                                                )
import           Domain.Services.LogMonad       ( Log(writeLogE, writeLogD) )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Adapter.PostgreSQL.ImportLibrary
                                                ( query
                                                , sql
                                                , Only(Only)
                                                )


checkAdminAccess :: PG r m => SessionId -> m ()
checkAdminAccess sesId = do
  idU         <- findUserIdBySession sesId
  resultAdmin <- withConn $ \conn -> query conn qry idU :: IO [Only Bool]
  case resultAdmin of
    [Only True] -> do
      writeLogD "checkAdminAccess True "
      return ()
    [Only False] -> do
      writeLogD "checkAdminAccess False "
      throwError NotAccessNotAdmid
    _ -> do
      writeLogE "(errorText DataErrorPostgreSQL) "
      throwError DataErrorPostgreSQL
  where qry = [sql|select admin from usernews where id_user = ? ;|]

checkAuthorAccess :: PG r m => SessionId -> m ()
checkAuthorAccess sesId = do
  idA          <- findUserIdBySession sesId
  resultAuthor <- withConn $ \conn -> query conn qry [idA] :: IO [Only Bool]
  case resultAuthor of
    [Only True] -> do
      writeLogD "checkAuthorAccess True "
      return ()
    [Only False] -> do
      writeLogD "checkAuthorAccess False "
      throwError NotAccessNotAuthor
    _ -> do
      writeLogE "checkAuthorAccess DataErrorPostgreSQL "
      throwError DataErrorPostgreSQL
  where qry = [sql| select authoris from usernews where id_user = ? ;|]

getAuthorId :: PG r m => SessionId -> m Int
getAuthorId sesId = do
  idA          <- findUserIdBySession sesId
  resultAuthor <- withConn $ \conn -> query conn qry [idA] :: IO [Only Int]
  case resultAuthor of
    [Only idUserAuthor] -> do
      writeLogD "getAuthorId "
      return idUserAuthor
    _ -> throwError DataErrorPostgreSQL
  where qry = [sql| select id_author from author where id_link_user = ? ;|]
