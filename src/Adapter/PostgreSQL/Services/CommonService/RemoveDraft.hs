module Adapter.PostgreSQL.Services.CommonService.RemoveDraft where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude ( ($), Monad(return), Int, (++) ) 
import Domain.Types.ImportTypes
  ( ErrorServer(DataErrorPostgreSQL, ErrorTakeEntityNotSupposed)
  , HelpForRequest(CommentEntReq, DraftEntReq)
  , LogLevel(Debug, ErrorLog)
  , UserId
  , errorText
  )
import Control.Monad.Except ( MonadError(throwError) )
import Domain.Services.LogMonad (Log(writeLog))

import Database.PostgreSQL.Simple (execute)

removeDraft ::
     PG r m =>  Int ->  UserId -> m ()
removeDraft idEnt idA  = do
      let q =
            "DELETE FROM draft WHERE id_author_draft = (select id_link_user from author where id_author = (?) ) and id_draft = (?);"
      result <- withConn $ \conn -> execute conn q (idA, idEnt)
      case result of
        1 -> do
          writeLog Debug "delete draft good!"
          return  ()
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " delete draft")
          throwError DataErrorPostgreSQL