module Adapter.PostgreSQL.Services.SortedService where

import Adapter.PostgreSQL.Common (PG, requestForPost, withConn)
import ClassyPrelude
  ( Either(..)
  , Eq((==))
  , IO
  , Monad(return)
  , Text
  , ($)
  , (++)
  , map
  , otherwise
  )

import Database.PostgreSQL.Simple (query_)
import Domain.Services.LogMonad (Log(writeLog))
import Domain.Types.ImportTypes
  ( ErrorServer(DataErrorPostgreSQL, ErrorTakeEntityNotSupposed)
  , LogLevel(Debug, ErrorLog)
  , News
  , NewsRaw
  , convertNewsRaw
  , errorText
  )

sortedNews :: PG r m => Text -> m (Either ErrorServer [News])
sortedNews txtCond -- txtCond - text condition
  | txtCond == "date" = sortedDate
  | txtCond == "author" = sortedAuthor
  | txtCond == "category" = sortedCategory
  | txtCond == "photo" = sortedPhoto
  | otherwise = return $ Left ErrorTakeEntityNotSupposed

sortedDate :: PG r m => m (Either ErrorServer [News])
sortedDate = do
  let q = requestForPost ++ " ORDER BY data_creat_news limit 20;"
  result <- withConn $ \conn -> query_ conn q :: IO [NewsRaw]
  case result of
    [] -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " sortedDate")
      return $ Left DataErrorPostgreSQL
    news -> do
      writeLog Debug "sortedDate success "
      return $ Right $ map convertNewsRaw news

sortedAuthor :: PG r m => m (Either ErrorServer [News])
sortedAuthor = do
  let q = requestForPost ++ " ORDER BY authors_id_news limit 20;"
  result <- withConn $ \conn -> query_ conn q :: IO [NewsRaw]
  case result of
    [] -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " sortedAuthor")
      return $ Left DataErrorPostgreSQL
    news -> do
      writeLog Debug "sortedAuthor success "
      return $ Right $ map convertNewsRaw news

sortedCategory :: PG r m => m (Either ErrorServer [News])
sortedCategory = do
  let q = requestForPost ++ " ORDER BY endNews.category_id_news limit 20;"
  result <- withConn $ \conn -> query_ conn q :: IO [NewsRaw]
  case result of
    [] -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " sortedCategory")
      return $ Left DataErrorPostgreSQL
    news -> do
      writeLog Debug "sortedCategory success "
      return $ Right $ map convertNewsRaw news

sortedPhoto :: PG r m => m (Either ErrorServer [News])
sortedPhoto = do
  let q = requestForPost ++ " ORDER BY other_photo_news limit 20"
  result <- withConn $ \conn -> query_ conn q :: IO [NewsRaw]
  case result of
    [] -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " sortedPhoto")
      return $ Left DataErrorPostgreSQL
    news -> do
      writeLog Debug "sortedPhoto success "
      return $ Right $ map convertNewsRaw news
--  Здесь можно еще добавить в запрос к базе DESC или ASC - это или ввести новую переменную(что предпочтительнее - так как нельзя 
-- будет ошибиться) или добавить еще варианты txt - тут опять же плохая масштабируемость, но в задании не говорили конкретно как 
-- сортировать)
