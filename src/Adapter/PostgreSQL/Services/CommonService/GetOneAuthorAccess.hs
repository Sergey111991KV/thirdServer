{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.CommonService.GetOneAuthorAccess where

import           Adapter.PostgreSQL.Common      ( PG
                                                , withConn
                                                )
import           ClassyPrelude                  ( ($)
                                                , Monad(return)
                                                , Int
                                                , IO
                                                )
import           Domain.Types.ExportTypes       ( ErrorServer
                                                  ( DataErrorPostgreSQL
                                                  )
                                                , UserId
                                                , Draft
                                                )

import           Control.Monad.Except           ( MonadError(throwError) )
import           Domain.Services.LogMonad       ( Log(writeLogE, writeLogD) )
import           Adapter.PostgreSQL.ImportLibrary
                                                ( encode
                                                , query
                                                , sql
                                                )
import qualified Data.ByteString.Lazy.Internal as LB

getOneAuthorAccess :: PG r m => Int -> UserId -> m LB.ByteString
getOneAuthorAccess idE idA = do
  resultDraft <- withConn $ \conn -> query conn qry (idA, idE) :: IO [Draft]
  case resultDraft of
    [x] -> do
      writeLogD "getOne Draft success!"
      return $ encode x
    _ -> do
      writeLogE "getOneDraft DataErrorPostgreSQL "
      throwError DataErrorPostgreSQL
 where
  qry = [sql| SELECT    draft.id_draft, 
                              draft.text_draft, 
                              draft.data_create_draft, 
                              draft.news_id_draft,
                              draft.main_photo_draft, 
                              draft.short_name_draft, 
                              draft.other_photo_draft, 
                              draft.tags_id, 
                              draft.id_author_draft 
                              from draft, author where draft.id_author_draft= author.id_author  
                              and author.id_link_user = (?) 
                              and draft.id_draft= (?); |]

