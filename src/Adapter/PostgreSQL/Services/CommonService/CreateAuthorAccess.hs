{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.CommonService.CreateAuthorAccess where

import           Adapter.PostgreSQL.Common      ( PG
                                                , withConn
                                                )
import           ClassyPrelude                  ( ($)
                                                , Monad(return)
                                                )
import           Domain.Types.ExportTypes      

import           Adapter.PostgreSQL.ImportLibrary
                                                ( execute
                                                , sql
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Domain.Services.LogMonad       ( Log(writeLogE, writeLogD) )


createAuthorAccess :: PG r m => AnEntity  -> m ()
createAuthorAccess (AnDraft draft) = do
  result <- withConn $ \conn -> execute
    conn
    [sql| INSERT INTO draft (text_draft, data_create_draft, 
                                    news_id_draft, main_photo_draft,other_photo_draft, 
                                    short_name_draft,tags_id, id_author_draft) 
                                    VALUES (?,?,?,?,?,?,?,?);
                |]
    ( textDraft draft
    , dataCreateDraft draft
    , newsIdDraft draft
    , mainPhotoUrl draft
    , otherPhotoUrl draft
    , shortNameDraft draft
    , tagsId draft
    , idAuthorDraft draft
    )
  case result of
    1 -> do
      writeLogD "create draft good!"
      return ()
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
createAuthorAccess _  = do
  writeLogE (errorText DataErrorPostgreSQL)
  throwError DataErrorPostgreSQL
