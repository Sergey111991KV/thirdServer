{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.SortedService where

import           Adapter.PostgreSQL.Common
import           ClassyPrelude
import           Control.Monad.Except           ( MonadError(throwError) )
import           Database.PostgreSQL.Simple
import           Domain.Services.LogMonad       ( Log(writeLogD, writeLogE) )
import           Domain.Types.ExportTypes
import           Adapter.PostgreSQL.ImportLibrary
import qualified Data.ByteString.Lazy.Internal as LB

getQueryOfsortedDate ::  MonadError ErrorServer m => Text -> m Query 
getQueryOfsortedDate txtCondSort = do
  case txtCondSort of 
    "DESC" -> return [sql| select    endNews.id_news 
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
	 			               from (select * from news left join author on author.id_author = news.authors_id_news ) as endNews ORDER BY data_creat_news DESC limit 20 offset (?);|]
    "ASC" -> return [sql| select    endNews.id_news 
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
	 			               from (select * from news left join author on author.id_author = news.authors_id_news ) as endNews ORDER BY data_creat_news ASC limit 20 offset (?);|]
    _  -> throwError ErrorGetPageQueryConvertText

sortedDate :: PG r m =>  Text -> Int -> m LB.ByteString
sortedDate txtCondSort page = do
  q <- getQueryOfsortedDate txtCondSort
  result <- withConn $ \conn -> query conn q [page] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " sortedDate")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "sortedDate success "
      return $ encode $ map convertNewsRaw news

sortedAuthor :: PG r m => Int -> m LB.ByteString
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
      return $ encode $ map convertNewsRaw news

sortedCategory :: PG r m => Int -> m LB.ByteString
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
      return $ encode $ map convertNewsRaw news

sortedPhoto :: PG r m => Int -> m LB.ByteString
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
      return $ encode $ map convertNewsRaw news

--  Здесь можно еще добавить в запрос к базе DESC или ASC - это или ввести новую переменную(что предпочтительнее - так как нельзя 
-- будет ошибиться) или добавить еще варианты txt - тут опять же плохая масштабируемость, но в задании не говорили конкретно как 
-- сортировать)
