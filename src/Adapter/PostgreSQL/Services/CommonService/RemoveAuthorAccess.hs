module Adapter.PostgreSQL.Services.CommonService.RemoveAuthorAccess where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude ( ($), Monad(return), Int, (++) ) 
import Domain.Types.ImportTypes
    ( errorText, ErrorServer(DataErrorPostgreSQL), UserId )
import Control.Monad.Except ( MonadError(throwError) )
import Domain.Services.LogMonad ( Log(writeLogE, writeLogD) ) 

import Database.PostgreSQL.Simple (execute)

removeAuthorAccess ::
     PG r m =>  Int ->  UserId -> m ()
removeAuthorAccess idEnt idA  = do
      let q =
            "DELETE FROM draft WHERE id_author_draft = (select id_link_user from author where id_author = (?) ) and id_draft = (?);"
      result <- withConn $ \conn -> execute conn q (idA, idEnt)
      case result of
        1 -> do
          writeLogD "delete draft good!"
          return  ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL ++ " delete draft")
          throwError DataErrorPostgreSQL