{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.CommonService.GetAll where

import           Adapter.PostgreSQL.Common      
import           ClassyPrelude

import           Database.PostgreSQL.Simple
import           Domain.Services.LogMonad       ( Log(writeLogE, writeLogD) )
import           Domain.Types.ExportTypes
import           Adapter.PostgreSQL.ImportLibrary
import           Control.Monad.Except           ( MonadError(throwError) )
import qualified Data.ByteString.Lazy.Internal as LB

getAll :: PG r m => HelpForRequest -> Int -> m LB.ByteString
getAll helpEnt p = do
  let page = p * 20
  case helpEnt of
    AuthorEntReq -> do
      let qAuthor = [sql| SELECT * from author limit 20 offset (?);|]
      result <- withConn $ \conn -> query conn qAuthor [page] :: IO [Author]
      case result of
        [] -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
        authorsArray -> do
          writeLogD "gett all author success!"
          return $ encode authorsArray
    UserEntReq -> do
      let qUser = [sql| SELECT * from usernews limit 20 offset (?);|]
      result <- withConn $ \conn -> query conn qUser [page] :: IO [User]
      case result of
        [] -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
        users -> do
          writeLogD "gett all user success!"
          return $ encode users
    TagEntReq -> do
      let qTag = [sql| SELECT * from tag limit 20 offset (?);|]
      result <- withConn $ \conn -> query conn qTag [page] :: IO [Tag]
      case result of
        [] -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
        users -> do
          writeLogD "gett all Tag success!"
          return $ encode users
    CategoryEntReq -> do
      let
        qCat =
          [sql| with recursive temp1 (id_category, parent_category, name_category) as ( 
                                     select t1.id_category, t1.parent_category, t1.name_category 
                                     from category t1  
                                     union 
                                     select t2.id_category, t2.parent_category, t2.name_category 
                                     from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) 
                                     SELECT distinct id_category, name_category, parent_category from temp1 limit 20 offset (?);|]
      result <- withConn $ \conn -> query conn qCat [page] :: IO [CategoryRaw]
      case result of
        [] -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
        cat -> do
          writeLogD "gett all Category success!"
          return . encode $ convertCategoryRawArray cat
    NewsEntReq -> do
      let qDraft = [sql| select    endNews.id_news 
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
	 			               from (select * from news left join author on author.id_author = news.authors_id_news ) as endNews limit 20 offset (?);|]
      liftIO $ print qDraft
      result <- withConn $ \conn -> query conn qDraft [page] :: IO [NewsRaw]
      case result of
        [] -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
        news -> do
          writeLogD "gett all News success!"
          return . encode $ map convertNewsRaw news
    _ -> do
      writeLogE (errorText ErrorTakeEntityNotSupposed)
      throwError ErrorTakeEntityNotSupposed
