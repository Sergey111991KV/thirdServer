module Domain.Services.Auth where

import ClassyPrelude (Either(..), Monad(return), ($))

import Domain.Services.LogMonad (Log(..))
import Domain.Types.ImportTypes
  ( ErrorServer
  , LogLevel(Debug, ErrorLog)
  , Login
  , Password
  , SessionId
  , UserId
  , errorText
  )

class (Log m) =>
      Auth m
  where
  findUserId :: Login -> Password -> m (Either ErrorServer UserId)
  newSession :: UserId -> m (Either ErrorServer SessionId)
  findUserIdBySession :: SessionId -> m (Either ErrorServer UserId)
  deleteOldSession :: UserId -> m (Either ErrorServer ())

sessionByAuth :: Auth m => Login -> Password -> m (Either ErrorServer SessionId)
sessionByAuth loggin password = do
  uIdResult <- findUserId loggin password
  case uIdResult of
    Left errGetSession -> do
      writeLog ErrorLog (errorText errGetSession)
      return $ Left errGetSession
    Right uId -> do
      writeLog Debug "sessionByAuth is OK"
      newSession uId
