{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.CommonService.Publish where

import           Adapter.PostgreSQL.Common      ( PG
                                                , withConn
                                                )
import ClassyPrelude ( ($), Monad(return), Int, IO )                  
import Domain.Services.LogMonad
    ( Log(writeLogE, writeLogD, writeLog) )
import Domain.Types.ExportTypes
    ( errorText,
      ErrorServer(DataErrorPostgreSQL),
      LogWrite(Debug),
      UserId )
import Adapter.PostgreSQL.ImportLibrary ( query, sql, Only(Only) )
import           Control.Monad.Except           ( MonadError(throwError) )

publish :: PG r m => UserId -> Int -> m ()
publish idU idE = do
  let qAuthor = [sql| select id_author from author where id_link_user= (?); |]
  resultAuthor <- withConn $ \conn -> query conn qAuthor idU :: IO [Only Int]
  case resultAuthor of
    [Only x] -> do
      writeLogD "getOne News success!"
      -- let qPublic = [sql| select updeite_news (?, ?); |]
      let   qPublic    = [sql| select tag_news_insert (tags_id,updeite_news_with_tags((?),(?))) from draft where draft.id_draft = (?) ;  |]
      _ <- withConn
        $ \conn -> query conn qPublic (x, idE, idE) :: IO [Only Int]
      writeLog Debug "publish Draft success!"
      return ()
    _ -> do
      writeLogE (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
                                            -- здесь конечно нужно запросы обьединить в один - я такое уже сделал в других местах,
                                            -- хотя с другой стороны - здесь будут выходить более явные ошибки

