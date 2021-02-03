module Adapter.PostgreSQL.Services.CommonService.Publish where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude ( ($), Monad(return), Int, IO, (++) ) 
import Database.PostgreSQL.Simple (Only(Only), query)
import Domain.Services.LogMonad (Log(writeLog))
import Domain.Types.ImportTypes
  ( ErrorServer(DataErrorPostgreSQL)
  , LogLevel(Debug, ErrorLog)
  , UserId
  , errorText
  )
import Control.Monad.Except ( MonadError(throwError) )

publish :: PG r m => UserId -> Int -> m ()
publish idU idE = do
  let qAuthor = "select id_author from author where id_link_user= (?);"
  resultAuthor <- withConn $ \conn -> query conn qAuthor idU :: IO [Only Int]
  case resultAuthor of
    [Only x] -> do
      writeLog Debug "getOne News success!"
      let qPublich = "select updeite_news (?, ?);"
      resultPublic <-
        withConn $ \conn -> query conn qPublich (x, idE) :: IO [Only Int]
      case resultPublic of
        [Only 1] -> do
          writeLog Debug "publish Draft success!"
          return  ()
        _ -> do
          writeLog
            ErrorLog
            (errorText DataErrorPostgreSQL ++ " can't to publish news")
          throwError DataErrorPostgreSQL
    _ -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL)
      throwError DataErrorPostgreSQL
                                            -- здесь конечно нужно запросы обьединить в один - я такое уже сделал в других местах,
                                            -- хотя с другой стороны - здесь будут выходить более явные ошибки
