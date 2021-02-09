module Adapter.PostgreSQL.Services.CommonService.GetAllAuthorAccess where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude
    ( ($), Monad(return), Functor(fmap), IO, (++), null )
import Database.PostgreSQL.Simple (query)
import Domain.Services.LogMonad ( Log(writeLogD, writeLogE) ) 
import Domain.Types.ImportTypes
    ( errorText,
      ErrorServer(DataErrorPostgreSQL),
      UserId,
      Draft,
      AnEntity(..) )
import Control.Monad.Except ( MonadError(throwError) ) 

getAllAuthorAccess :: PG r m => UserId -> m [AnEntity]
getAllAuthorAccess uId = do
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
      writeLogE $ errorText DataErrorPostgreSQL ++ " not draft "
      throwError DataErrorPostgreSQL
    else do
      writeLogD "Get all draft for user"
      return (fmap AnEntity result)
