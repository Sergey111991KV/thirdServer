module Domain.Services.Auth where

import           ClassyPrelude

import           Domain.Services.LogMonad       ( Log(..) )
import           Domain.Types.ExportTypes
import           Domain.Services.EntityService

class (Log m,  Entity m) =>
      Auth m
  where
  findUserId :: Login -> Password -> m  UserId
  newSession :: UserId -> m SessionId
  findUserIdBySession :: SessionId -> m UserId
  deleteOldSession :: UserId -> m  ()

sessionByAuth :: Auth m => Login -> Password -> m SessionId
sessionByAuth loggin password = do
  uIdResult <- findUserId loggin password
  newSession uIdResult

exitSession :: Auth m => SessionId -> m ()
exitSession sess = findUserIdBySession sess >>= deleteOldSession
