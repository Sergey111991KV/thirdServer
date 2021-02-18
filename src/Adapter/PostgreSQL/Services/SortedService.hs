module Adapter.PostgreSQL.Services.SortedService where

import Adapter.PostgreSQL.Common (PG, requestForPost, withConn)
import ClassyPrelude
    ( otherwise,
      ($),
      Eq((==)),
      Monad(return),
      Int,
      IO,
      Text,
      (++),
      map )
    
import Control.Monad.Except ( MonadError(throwError) )
import Database.PostgreSQL.Simple 
import Domain.Services.LogMonad ( Log(writeLogD, writeLogE) ) 
import Domain.Types.ExportTypes
import Adapter.PostgreSQL.ImportLibrary
  

sortedNews :: PG r m => Text -> Int -> m  [News]
sortedNews txtCond page
  | txtCond == "date" = sortedDate page
  | txtCond == "author" = sortedAuthor page
  | txtCond == "category" = sortedCategory page
  | txtCond == "photo" = sortedPhoto page
  | otherwise = throwError ErrorTakeEntityNotSupposed

sortedDate :: PG r m => Int -> m [News]
sortedDate page = do
  let q = requestForPost ++ " ORDER BY data_creat_news limit 20 offset (?);"
  result <- withConn $ \conn -> query conn q [page] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " sortedDate")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "sortedDate success "
      return $ map convertNewsRaw news

sortedAuthor :: PG r m => Int -> m [News]
sortedAuthor page = do
  let q = requestForPost ++ " ORDER BY authors_id_news limit 20 offset (?);"
  result <- withConn $ \conn -> query conn q [page] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " sortedAuthor")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "sortedAuthor success "
      return $ map convertNewsRaw news

sortedCategory :: PG r m => Int -> m [News]
sortedCategory page = do
  let q = requestForPost ++ " ORDER BY endNews.category_id_news limit 20 offset (?);"
  result <- withConn $ \conn -> query conn q [page] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " sortedCategory")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "sortedCategory success "
      return $ map convertNewsRaw news

sortedPhoto :: PG r m => Int -> m [News]
sortedPhoto page = do
  let q = requestForPost ++ " ORDER BY other_photo_news limit 20 offset (?);"
  result <- withConn $ \conn -> query conn q [page] :: IO [NewsRaw]
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
