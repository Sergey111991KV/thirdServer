module Adapter.PostgreSQL.Services.CommonService.GetAllDraft where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude
import Database.PostgreSQL.Simple (query)
import Domain.Services.LogMonad (Log(writeLog))
import Domain.Types.ImportTypes
import Control.Monad.Except ( MonadError(throwError) ) 

getAllDraft :: PG r m => UserId -> m [AnEntity]
getAllDraft uId = do
  let q =
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
                                                            \    and author.id_link_user = (?) limit 20;"
  result <- withConn $ \conn -> query conn q [uId] :: IO [Draft]
  if null result
    then do
      writeLog ErrorLog $ errorText DataErrorPostgreSQL ++ " not draft "
      throwError DataErrorPostgreSQL
    else do
      writeLog Debug "Get all draft for user"
      return (fmap AnEntity result)
