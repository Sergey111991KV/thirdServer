{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.FilterService where

import           Adapter.PostgreSQL.Common
import           ClassyPrelude
import           Domain.Services.LogMonad
import           Domain.Types.ExportTypes
import           Adapter.PostgreSQL.ImportLibrary
import           Control.Monad.Except           ( MonadError(throwError) )
import qualified Prelude                       as P
import qualified Data.ByteString.Lazy.Internal as LB


filterOfData :: PG r m => Text -> Text -> Int -> m LB.ByteString
filterOfData condition time page = do
  let
    q =
      " select    endNews.id_news \
			              \  , endNews.data_creat_news \
	 			            \  , endNews.id_author \
	 			            \  , endNews.id_link_user \
			              \  , endNews.description \
	 			            \  , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path \
                    \  from news, category t1 where t1.id_category = endnews.category_id_news union \
                    \  select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) \
                    \  from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) \
                    \  select distinct (id_category, name_category, parent_category) from temp1) \
                    \  , endNews.text_news \
	 			            \  , endNews.main_photo_news \
	 			            \  , endNews.other_photo_news \
				            \  , endNews.short_name_news \
                    \  , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) \
	 			            \  , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) \
	 			            \   from (select * from news left join author on author.id_author = news.authors_id_news ) as endNews  "
        <> conversCond condition
        <> " limit 20 offset (?);"
  result <- withConn $ \conn -> query conn q (time, page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterOfData")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterOfData success "
      return $ encode $ map convertNewsRaw news

conversCond :: Text -> Query
conversCond txtCond | txtCond == "less"  = " where data_creat_news <= (?)"
                    | txtCond == "more"  = " where data_creat_news >= (?)"
                    | txtCond == "equel" = " where data_creat_news == (?)"
                    | otherwise          = "Error"

filterAuthor :: PG r m => Int -> Int -> m LB.ByteString
filterAuthor idA page = do
  let q = [sql| select  distinct  endNews.id_news 
				                        , endNews.data_creat_news 
				                        , endNews.id_author 
				                        , endNews.id_link_user 
				                        , endNews.description 
				                        , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( 
                                select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path 
                                from news, category t1 where t1.id_category = endnews.category_id_news 
                                union 
                                select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) 
                                from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                                select distinct (id_category, name_category, parent_category) from temp1) 
                                , endNews.text_news 
				                        , endNews.main_photo_news 
				                        , endNews.other_photo_news 
				                        , endNews.short_name_news 
                                , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) 
				                        , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) 
				                        from tags_news, (select * from news left join author on author.id_author = news.authors_id_news ) as endNews where endNews.id_author = (?) limit 20 offset (?);|]
  result <- withConn $ \conn -> query conn q (idA, page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterAuthor")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterAuthor success "
      return $ encode $ map convertNewsRaw news

filterCategory :: PG r m => Int -> Int -> m LB.ByteString
filterCategory catId page = do
  let q = [sql| select  distinct  endNews.id_news 
				                        , endNews.data_creat_news 
				                        , endNews.id_author 
				                        , endNews.id_link_user 
				                        , endNews.description 
				                        , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( 
                                select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path 
                                from news, category t1 where t1.id_category = endnews.category_id_news 
                                union 
                                select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) 
                                from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                                select distinct (id_category, name_category, parent_category) from temp1) 
                                , endNews.text_news 
				                        , endNews.main_photo_news 
				                        , endNews.other_photo_news 
				                        , endNews.short_name_news 
                                , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) 
				                        , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) 
				                        from tags_news, (select * from news left join author on author.id_author = news.authors_id_news ) as endNews where endNews.category_id_news = (?) limit 20 offset (?);|]
  result <- withConn $ \conn -> query conn q (catId, page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterCategory")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterCategory success "
      return $ encode $ map convertNewsRaw news

filterTag :: PG r m => Int -> Int -> m LB.ByteString
filterTag idT page = do
  let q = [sql| select  distinct  endNews.id_news 
				                      , endNews.data_creat_news 
				                      , endNews.id_author 
				                      , endNews.id_link_user 
				                      , endNews.description 
				                      , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( 
                              select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path 
                              from news, category t1 where t1.id_category = endnews.category_id_news 
                              union 
                              select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) 
                              from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                              select distinct (id_category, name_category, parent_category) from temp1) 
                              , endNews.text_news 
				                      , endNews.main_photo_news 
				                      , endNews.other_photo_news 
				                      , endNews.short_name_news 
                              , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) 
				                      , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL ) as t) 
				                       from  ( select * from (select * from news left join author on author.id_author = news.authors_id_news ) as endN right join tags_news 
				                       on 
				                       tags_news.news_id = endN.id_news and  
				                       tags_news.tags_id = (?) ) as  endNews where endNews.id_news  IS NOT NULL limit 20 offset (?);|]
  result <- withConn $ \conn -> query conn q (idT, page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterTeg")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterTeg success "
      return $ encode $ map convertNewsRaw news

filterOneOfTags :: PG r m => Text -> Int -> m LB.ByteString
filterOneOfTags idTarray page = do
  let reqArr = "{" ++ idTarray ++ "}"
  let q = [sql| select  distinct  endNews.id_news 
				                      , endNews.data_creat_news 
				                      , endNews.id_author 
				                      , endNews.id_link_user 
				                      , endNews.description 
				                      , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( 
                              select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path 
                              from news, category t1 where t1.id_category = endnews.category_id_news union 
                              select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) 
                              from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                              select distinct (id_category, name_category, parent_category) from temp1) 
                              , endNews.text_news 
				                      , endNews.main_photo_news 
				                      , endNews.other_photo_news 
				                      , endNews.short_name_news 
                              , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) 
				                      , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL ) as t) 
				                       from  ( select * from (select * from news left join author on author.id_author = news.authors_id_news ) as endN right join tags_news on 
				                       tags_news.news_id = endN.id_news and  
				                       tags_news.tags_id = any (?) ) as  endNews where endNews.id_news  IS NOT NULL limit 20 offset (?);|]
  result <- withConn $ \conn -> query conn q (reqArr, page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterOneOfTags")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterOneOfTags success "
      return $ encode $ map convertNewsRaw news

filterAllOfTags :: PG r m => Text -> Int -> m LB.ByteString
filterAllOfTags idTarray page = do
  let reqArr = createAllTagRequest $ unpack idTarray
  let q = [sql| select  distinct  endNews.id_news 
				                        , endNews.data_creat_news 
				                        , endNews.id_author 
				                        , endNews.id_link_user 
				                        , endNews.description 
				                        , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( 
                                select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path 
                                from news, category t1 where t1.id_category = endnews.category_id_news 
                                union 
                                select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) 
                                from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                                select distinct (id_category, name_category, parent_category) from temp1) 
                                , endNews.text_news 
				                        , endNews.main_photo_news 
				                        , endNews.other_photo_news 
				                        , endNews.short_name_news 
                                , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) 
				                        , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) 
				                        from  (select * from (select news_id,  row(array_agg(distinct tags_id)) as d from 
 				                        tags_news  GROUP BY news_id  ) as e  right join (select * from news left join author on author.id_author = news.authors_id_news ) as n ON e.news_id = n.id_news) as endNews 
                                where (endNews.d :: text) LIKE ((?) :: text) limit 20  offset (?);|]
  result <- withConn $ \conn -> query conn q (reqArr, page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterAllOfTags")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterAllOfTags success "
      return $ encode $ map convertNewsRaw news

createAllTagRequest :: String -> String
createAllTagRequest = createAllTagRequest' "%"
 where
  createAllTagRequest' arr []         = arr
  createAllTagRequest' arr (',' : xs) = createAllTagRequest' arr xs
  createAllTagRequest' arr (x : xs) =
    createAllTagRequest' (arr ++ [x] ++ "%") xs

filterName :: PG r m => Text -> Int -> m LB.ByteString
filterName txtName page = do
  let q = [sql| select  distinct  endNews.id_news 
				                        , endNews.data_creat_news 
				                        , endNews.id_author 
				                        , endNews.id_link_user 
				                        , endNews.description 
				                        , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( 
                                select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path 
                                from news, category t1 where t1.id_category = endnews.category_id_news 
                                union 
                                select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) 
                                from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                                select distinct (id_category, name_category, parent_category) from temp1) 
                                , endNews.text_news 
				                        , endNews.main_photo_news 
				                        , endNews.other_photo_news 
				                        , endNews.short_name_news 
                                , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) 
				                        , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) 
				                        from tags_news, (select * from news left join author on author.id_author = news.authors_id_news ) as endNews where endNews.short_name_news LIKE (?) limit 20 offset (?);|]
  let insertText = "%" ++ txtName ++ "%"
  -- let q = requestForPost ++ " where endNews.short_name_news LIKE (?) limit 20 offset (?);"
  result <- withConn $ \conn -> query conn q (insertText, page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterName")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterName success "
      return $ encode $ map convertNewsRaw news

filterContent :: PG r m => Text -> Int -> m LB.ByteString
filterContent txtContent page = do
  let insertText = "%" ++ txtContent ++ "%"
  let q = [sql| select  distinct  endNews.id_news 
				                        , endNews.data_creat_news 
				                        , endNews.id_author 
				                        , endNews.id_link_user 
				                        , endNews.description 
				                        , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( 
                                select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path 
                                from news, category t1 where t1.id_category = endnews.category_id_news 
                                union 
                                select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) 
                                from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                                select distinct (id_category, name_category, parent_category) from temp1) 
                                , endNews.text_news 
				                        , endNews.main_photo_news 
				                        , endNews.other_photo_news 
				                        , endNews.short_name_news 
                                , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) 
				                        , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) 
				                        from tags_news, (select * from news left join author on author.id_author = news.authors_id_news ) as endNews where endNews.text_news LIKE (?) limit 20 offset (?);|]
  result <- withConn $ \conn -> query conn q (insertText, page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterContent")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterContent success "
      return $ encode $ map convertNewsRaw news

toStringFromArrayInt :: [Int] -> Text
toStringFromArrayInt array = pack $ "{" ++ P.foldl addParam "" array ++ "}"
 where
  addParam []       arr = show arr
  addParam elements arr = elements ++ (',' : show arr)
