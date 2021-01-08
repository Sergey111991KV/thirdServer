module Adapter.PostgreSQL.Services.CommonService.RemoveDraft where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude (Either(..), Int, Monad(return), ($), (++))
import Domain.Types.ImportTypes
  ( ErrorServer(DataErrorPostgreSQL, ErrorTakeEntityNotSupposed)
  , HelpForRequest(CommentEntReq, DraftEntReq)
  , LogLevel(Debug, ErrorLog)
  , UserId
  , errorText
  )

import Domain.Services.LogMonad (Log(writeLog))

import Database.PostgreSQL.Simple (execute)

removeDraft ::
     PG r m => HelpForRequest -> UserId -> Int -> m (Either ErrorServer ())
removeDraft helpR uId idEnt = do
  case helpR of
    CommentEntReq -> do
      let q =
            "DELETE FROM comment WHERE user_id_comment =(?) and id_comment = (?);"
      result <- withConn $ \conn -> execute conn q (uId, idEnt)
      case result of
        1 -> do
          writeLog Debug "delete comment good!"
          return $ Right ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " delete comment")
          return $ Left DataErrorPostgreSQL
    DraftEntReq -> do
      let q =
            "DELETE FROM draft WHERE id_author_draft = (select id_link_user from author where id_author = (?) ) and id_draft = (?);"
      result <- withConn $ \conn -> execute conn q (uId, idEnt)
      case result of
        1 -> do
          writeLog Debug "delete draft good!"
          return $ Right ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " delete draft")
          return $ Left DataErrorPostgreSQL
    _ -> do
      writeLog ErrorLog (errorText ErrorTakeEntityNotSupposed)
      return $ Left ErrorTakeEntityNotSupposed
