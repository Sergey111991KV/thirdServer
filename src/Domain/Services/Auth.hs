module Domain.Services.Auth where

import           ClassyPrelude                  ( Monad((>>=)) )

import           Domain.Services.LogMonad       ( Log(..) )
import           Domain.Types.ExportTypes       ( Login
                                                , Password
                                                , SessionId
                                                , UserId
                                                )
import           Domain.Services.EntityService  ( Entity )

class (Log m,  Entity m) =>
      Auth m
  where
  findUserId :: Login -> Password -> m  UserId
  newSession :: UserId -> m SessionId
  findUserIdBySession :: SessionId -> m UserId
  deleteOldSession :: UserId -> m  ()

sessionByAuth :: Auth m => Login -> Password -> m SessionId
sessionByAuth login password = do
  uIdResult <- findUserId login password
  newSession uIdResult

exitSession :: Auth m => SessionId -> m ()
exitSession sess = findUserIdBySession sess >>= deleteOldSession
