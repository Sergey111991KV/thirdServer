{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.SortedService where

import Adapter.PostgreSQL.Common 
import ClassyPrelude
import Control.Monad.Except ( MonadError(throwError) )
import Database.PostgreSQL.Simple ( query ) 
import Domain.Services.LogMonad ( Log(writeLogD, writeLogE) ) 
import Domain.Types.ExportTypes
    ( errorText,
      ErrorServer(DataErrorPostgreSQL, ErrorTakeEntityNotSupposed),
      convertNewsRaw,
      News,
      NewsRaw )
import Adapter.PostgreSQL.ImportLibrary


requestForPost' :: Query
requestForPost' = [sql| select    endNews.id_news 
			                , endNews.data_creat_news 
	 			              , endNews.id_author 
	 			              , endNews.id_link_user 
			                , endNews.description 
	 			              , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path 
                      from news, category t1 where t1.id_category = endnews.category_id_news union 
                      select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) 
                      from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                      select distinct (id_category, name_category, parent_category) from temp1) 
                      , endNews.text_news 
	 			              , endNews.main_photo_news 
	 			              , endNews.other_photo_news 
				              , endNews.short_name_news \
                      , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) 
	 			              , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) 
	 			               from (select * from news left join author on author.id_author = news.authors_id_news ) as endNews"
                      |]

sortedNews :: PG r m => Text -> Int -> m  [News]
sortedNews txtCond page
  | txtCond == "date" = sortedDate page
  | txtCond == "author" = sortedAuthor page
  | txtCond == "category" = sortedCategory page
  | txtCond == "photo" = sortedPhoto page
  | otherwise = throwError ErrorTakeEntityNotSupposed

sortedDate :: PG r m => Int -> m [News]
sortedDate page = do
  let q = [sql| select    endNews.id_news 
			                , endNews.data_creat_news 
	 			              , endNews.id_author 
	 			              , endNews.id_link_user 
			                , endNews.description 
	 			              , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path 
                      from news, category t1 where t1.id_category = endnews.category_id_news union 
                      select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) 
                      from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                      select distinct (id_category, name_category, parent_category) from temp1) 
                      , endNews.text_news 
	 			              , endNews.main_photo_news 
	 			              , endNews.other_photo_news 
				              , endNews.short_name_news 
                      , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) 
	 			              , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) 
	 			               from (select * from news left join author on author.id_author = news.authors_id_news ) as endNews ORDER BY data_creat_news limit 20 offset (?);|]
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
  let q = [sql| select    endNews.id_news 
			                , endNews.data_creat_news 
	 			              , endNews.id_author 
	 			              , endNews.id_link_user 
			                , endNews.description 
	 			              , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path 
                      from news, category t1 where t1.id_category = endnews.category_id_news union 
                      select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) 
                      from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                      select distinct (id_category, name_category, parent_category) from temp1) 
                      , endNews.text_news 
	 			              , endNews.main_photo_news 
	 			              , endNews.other_photo_news 
				              , endNews.short_name_news 
                      , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) 
	 			              , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) 
	 			               from (select * from news left join author on author.id_author = news.authors_id_news ) as endNews ORDER BY authors_id_news limit 20 offset (?);|]
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
  let q = [sql| select    endNews.id_news 
			                , endNews.data_creat_news 
	 			              , endNews.id_author 
	 			              , endNews.id_link_user 
			                , endNews.description 
	 			              , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path 
                      from news, category t1 where t1.id_category = endnews.category_id_news union 
                      select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) 
                      from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                      select distinct (id_category, name_category, parent_category) from temp1) 
                      , endNews.text_news 
	 			              , endNews.main_photo_news 
	 			              , endNews.other_photo_news 
				              , endNews.short_name_news 
                      , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) 
	 			              , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) 
	 			               from (select * from news left join author on author.id_author = news.authors_id_news ) as endNews ORDER BY endNews.category_id_news limit 20 offset (?); |]
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
  let q = [sql| select    endNews.id_news 
			                , endNews.data_creat_news 
	 			              , endNews.id_author 
	 			              , endNews.id_link_user 
			                , endNews.description 
	 			              , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path 
                      from news, category t1 where t1.id_category = endnews.category_id_news union 
                      select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) 
                      from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                      select distinct (id_category, name_category, parent_category) from temp1) 
                      , endNews.text_news 
	 			              , endNews.main_photo_news 
	 			              , endNews.other_photo_news 
				              , endNews.short_name_news 
                      , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) 
	 			              , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) 
	 			               from (select * from news left join author on author.id_author = news.authors_id_news ) as endNews ORDER BY other_photo_news limit 20 offset (?); |]
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
