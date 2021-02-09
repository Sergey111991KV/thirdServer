module Adapter.PostgreSQL.Services.CommonService.EditingAuthorAccess where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude ( ($), Monad(return), Maybe(Just, Nothing) )
import Domain.Types.ImportTypes
   
import Control.Monad.Except ( MonadError(throwError) )
import Database.PostgreSQL.Simple (execute)
import Domain.Services.LogMonad ( Log(writeLogE, writeLogD) ) 

editingAuthorAccess :: PG r m => AnEntity -> UserId-> m  ()
editingAuthorAccess (AnEntity ent) (UserId idU) = do
      let draft = (getData (AnEntity ent) :: Draft)
      let q =
            "UPDATE draft SET text_draft=(?), data_create_draft=(?), news_id_draft=(?), main_photo_draft=(?), other_photo_draft=(?), short_name_draft=(?), tags_id=(?), id_author_draft=(?) FROM author where id_draft =(?) and author.id_link_user =(?);"
      result <-
        withConn $ \conn ->
          execute
            conn
            q
            ( textDraft draft
            , dataCreateDraft draft
            , newsIdDraft draft
            , mainPhotoUrl draft
            , otherPhotoUrl draft
            , shortNameDraft draft
            , tagsId draft
            , idAuthorDraft draft
            , idDraft draft
            , idU)
      case result of
        1 -> do
          writeLogD "update draft good!"
          return ()
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL