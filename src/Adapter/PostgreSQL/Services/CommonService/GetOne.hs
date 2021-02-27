{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.CommonService.GetOne where

import           Adapter.PostgreSQL.Common      ( withConn
                                                , PG
                                                )
import           ClassyPrelude                  ( ($)
                                                , Monad(return)
                                                , Int
                                                , IO
                                                , (++)
                                                , null
                                                , head
                                                , impureNonNull
                                                )

import           Domain.Services.LogMonad       ( Log(writeLogE, writeLogD) )
import           Domain.Types.ExportTypes       ( errorText
                                                , ErrorServer
                                                  ( ErrorTakeEntityNotSupposed
                                                  , ErrorConvert
                                                  , DataErrorPostgreSQL
                                                  )
                                                , convertCategoryRawArray
                                                , CategoryRaw
                                                , Tag
                                                , Comment
                                                , Author
                                                , User
                                                , convertNewsRaw
                                                , NewsRaw
                                                , HelpForRequest
                                                  ( CategoryEntReq
                                                  , AuthorEntReq
                                                  , UserEntReq
                                                  , NewsEntReq
                                                  , TagEntReq
                                                  , CommentEntReq
                                                  )
                                                )

import           Control.Monad.Except           ( MonadError(throwError) )
import           Adapter.PostgreSQL.ImportLibrary
                                                ( encode
                                                , query
                                                , sql
                                                )
import qualified Data.ByteString.Lazy.Internal as LB



getOne :: PG r m => HelpForRequest -> Int -> m LB.ByteString
getOne helpR idE = do
  case helpR of
    AuthorEntReq -> do
      let qAuthor = [sql| SELECT * from author where id_author=(?)|]
      i <- withConn $ \conn -> query conn qAuthor [idE] :: IO [Author]
      case i of
        [x] -> do
          writeLogD "getOne Author success!"
          return $ encode x
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    UserEntReq -> do
      let qUser = [sql| SELECT * from usernews where id_user=(?)|]
      i <- withConn $ \conn -> query conn qUser [idE] :: IO [User]
      case i of
        [x] -> do
          writeLogD "getOne User success!"
          return $ encode x
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    NewsEntReq -> do
      let qNews = [sql| select    endNews.id_news 
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
	 			               from (select * from news left join author on author.id_author = news.authors_id_news ) as endNews where endNews.id_news = (?);|]
      i <- withConn $ \conn -> query conn qNews [idE] :: IO [NewsRaw]
      case i of
        [x] -> do
          writeLogD "getOne News success!"
          return $ encode $ convertNewsRaw x
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    TagEntReq -> do
      let qTag = [sql| SELECT * from tag where id_tag=(?);|]
      i <- withConn $ \conn -> query conn qTag [idE] :: IO [Tag]
      case i of
        [x] -> do
          writeLogD "getOne Tag success!"
          return $ encode x
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    CommentEntReq -> do
      let qComment = [sql| SELECT * from comment where id_comment=(?);|]
      i <- withConn $ \conn -> query conn qComment [idE] :: IO [Comment]
      case i of
        [x] -> do
          writeLogD "getOne Comment success!"
          return $ encode x
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    CategoryEntReq -> do
      let
        qCategory =
          [sql| with recursive temp1 (id_category, parent_category, name_category) as ( 
                                     select t1.id_category, t1.parent_category, t1.name_category 
                                     from category t1 where t1.id_category = (?) 
                                     union 
                                     select t2.id_category, t2.parent_category, t2.name_category 
                                     from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                                     SELECT distinct id_category, name_category, parent_category from temp1; |]
      i <- withConn $ \conn -> query conn qCategory [idE] :: IO [CategoryRaw]
      if null i
        then do
          writeLogE (errorText ErrorConvert ++ " finalCategoryConvert")
          throwError DataErrorPostgreSQL
        else do
          let cat = head $ impureNonNull $ convertCategoryRawArray i
          return $ encode cat
    _ -> do
      writeLogE (errorText ErrorTakeEntityNotSupposed)
      throwError ErrorTakeEntityNotSupposed
