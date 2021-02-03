module Adapter.PostgreSQL.Services.CommonService.GetOneDraft where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude (Either(..), IO, Int, Monad(return), ($))
import Domain.Types.ImportTypes
import Control.Monad.Except ( MonadError(throwError) )
import Domain.Services.LogMonad (Log(writeLog))
import Database.PostgreSQL.Simple (query)


getOneDraft :: PG r m => Int -> UserId -> m  AnEntity
getOneDraft idE idA = do
  resultDraft <- withConn $ \conn -> query conn qry (idA, idE) :: IO [Draft]
  case resultDraft of
    [x] -> do
          writeLog Debug "getOne Draft success!"
          return $ AnEntity x
    _ -> do
          writeLog ErrorLog "getOneDraft DataErrorPostgreSQL "
          throwError DataErrorPostgreSQL
  where qry =
              "SELECT  draft.id_draft, \
                           \ draft.text_draft, \
                           \ draft.data_create_draft, \
                           \ draft.news_id_draft, \
                           \ draft.main_photo_draft, \
                           \ draft.short_name_draft, \
                           \ draft.other_photo_draft, \
                           \ draft.tags_id, \
                           \ draft.id_author_draft \
                                 \   from draft, author where draft.id_author_draft= author.id_author  \
                                                            \    and author.id_link_user = (?) \
                                                            \    and draft.id_draft= (?);"
