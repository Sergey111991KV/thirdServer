{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.CommonService.GetAllAuthorAccess where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude
 
import Database.PostgreSQL.Simple
import Domain.Services.LogMonad ( Log(writeLogD, writeLogE) ) 
import Domain.Types.ExportTypes
import Adapter.PostgreSQL.ImportLibrary
import Control.Monad.Except ( MonadError(throwError) ) 
import qualified Data.ByteString.Lazy.Internal as LB

getAllAuthorAccess :: PG r m => UserId -> Int -> m LB.ByteString
getAllAuthorAccess uId page = do
  let q =
              [sql| SELECT  draft.id_draft, 
                            draft.text_draft, 
                            draft.data_create_draft, 
                            draft.news_id_draft, 
                            draft.main_photo_draft, 
                            draft.short_name_draft, 
                            draft.other_photo_draft, 
                            draft.tags_id, 
                            draft.id_author_draft 
                            from draft, author where draft.id_author_draft= author.id_author  
                            and author.id_link_user = (?) limit 20 offset (?); |]
  result <- withConn $ \conn -> query conn q (uId, page) :: IO [Draft]
  if null result
    then do
      writeLogE $ errorText DataErrorPostgreSQL ++ " not draft "
      throwError DataErrorPostgreSQL
    else do
      writeLogD "Get all draft for user"
      return $ encode result
