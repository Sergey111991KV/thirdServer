{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.CommonService.EditingAuthorAccess where

import           Adapter.PostgreSQL.Common      ( PG
                                                , withConn
                                                )
import           ClassyPrelude                  ( ($)
                                                , Monad(return)
                                                )
import           Domain.Types.ExportTypes       ( errorText
                                                , ErrorServer
                                                  ( DataErrorPostgreSQL
                                                  )
                                                , UserId(UserId)
                                                , Draft
                                                  ( textDraft
                                                  , dataCreateDraft
                                                  , newsIdDraft
                                                  , mainPhotoUrl
                                                  , otherPhotoUrl
                                                  , shortNameDraft
                                                  , tagsId
                                                  , idAuthorDraft
                                                  , idDraft
                                                  )
                                                , AnEntity(AnDraft)
                                                )

import           Adapter.PostgreSQL.ImportLibrary
                                                ( execute
                                                , sql
                                                )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Domain.Services.LogMonad       ( Log(writeLogE, writeLogD) )


editingAuthorAccess :: PG r m => AnEntity -> UserId -> m ()
editingAuthorAccess (AnDraft draft) (UserId idU) = do
  let
    q
      = [sql| UPDATE draft SET text_draft=(?), data_create_draft=(?), news_id_draft=(?), main_photo_draft=(?), other_photo_draft=(?), short_name_draft=(?), tags_id=(?), id_author_draft=(?) FROM author where id_draft =(?) and author.id_link_user =(?);|]
  result <- withConn $ \conn -> execute
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
    , idU
    )
  case result of
    1 -> do
      writeLogD "update draft good!"
      return ()
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
editingAuthorAccess _ _ = do
  writeLogE (errorText DataErrorPostgreSQL)
  throwError DataErrorPostgreSQL
