module Adapter.PostgreSQL.Services.SortedService where

import Adapter.PostgreSQL.Common (PG, requestForPost, withConn)
import ClassyPrelude
    ( otherwise, ($), Eq((==)), Monad(return), IO, Text, (++), map )
import Control.Monad.Except ( MonadError(throwError) )
import Database.PostgreSQL.Simple (query_)
import Domain.Services.LogMonad ( Log(writeLogD, writeLogE) ) 
import Domain.Types.ExportTypes
    ( errorText,
      ErrorServer(DataErrorPostgreSQL, ErrorTakeEntityNotSupposed),
      convertNewsRaw,
      News,
      NewsRaw )
  

sortedNews :: PG r m => Text -> m  [News]
sortedNews txtCond 
  | txtCond == "date" = sortedDate
  | txtCond == "author" = sortedAuthor
  | txtCond == "category" = sortedCategory
  | txtCond == "photo" = sortedPhoto
  | otherwise = throwError ErrorTakeEntityNotSupposed

sortedDate :: PG r m => m [News]
sortedDate = do
  let q = requestForPost ++ " ORDER BY data_creat_news limit 20;"
  result <- withConn $ \conn -> query_ conn q :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " sortedDate")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "sortedDate success "
      return $ map convertNewsRaw news

sortedAuthor :: PG r m => m [News]
sortedAuthor = do
  let q = requestForPost ++ " ORDER BY authors_id_news limit 20;"
  result <- withConn $ \conn -> query_ conn q :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " sortedAuthor")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "sortedAuthor success "
      return $ map convertNewsRaw news

sortedCategory :: PG r m => m [News]
sortedCategory = do
  let q = requestForPost ++ " ORDER BY endNews.category_id_news limit 20;"
  result <- withConn $ \conn -> query_ conn q :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " sortedCategory")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "sortedCategory success "
      return $ map convertNewsRaw news

sortedPhoto :: PG r m => m [News]
sortedPhoto = do
  let q = requestForPost ++ " ORDER BY other_photo_news limit 20"
  result <- withConn $ \conn -> query_ conn q :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " sortedPhoto")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "sortedPhoto success "
      return $ map convertNewsRaw news
      
--  Здесь можно еще добавить в запрос к базе DESC или ASC - это или ввести новую переменную(что предпочтительнее - так как нельзя 
-- будет ошибиться) или добавить еще варианты txt - тут опять же плохая масштабируемость, но в задании не говорили конкретно как 
-- сортировать)
