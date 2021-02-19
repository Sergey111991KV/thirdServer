{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Common where

import ClassyPrelude
import Control.Monad.Catch (MonadThrow, bracket)
import Data.Has (Has(getter))
import Data.Pool (Pool, createPool, destroyAllResources, withResource)
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple
import Domain.Types.ExportTypes
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand(MigrationDirectory, MigrationInitialization)
  , MigrationResult(MigrationError)
  , runMigrations
  )
import Domain.Services.Auth (Auth)
import qualified Domain.Services.LogMonad as Log
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad.Except 
import Control.Exception


type PG r m
   = (Has State r, MonadReader r m, MonadIO m, MonadThrow m, Auth m, Log.Log m)

type State = Pool Connection

data Config =
  Config
    { configUrl :: ByteString
    , configStripeCount :: Int
    , configMaxOpenConnPerStripe :: Int
    , configIdleConnTimeout :: NominalDiffTime
    }
  deriving (Show, Generic)

withPool :: Config -> (State -> IO a) -> IO a
withPool cfg = Control.Monad.Catch.bracket initPool cleanPool
  where
    initPool =
      createPool
        openConn
        closeConn
        (configStripeCount cfg)
        (configIdleConnTimeout cfg)
        (configMaxOpenConnPerStripe cfg)
    cleanPool = destroyAllResources
    openConn = connectPostgreSQL (configUrl cfg)
    closeConn = close

withState :: Config -> (State -> IO a) -> IO a
withState cfg action =
  withPool cfg $ \state -> do
    migrate state
    action state


withConn' :: Pool Connection -> (Connection -> IO a) -> IO a
withConn' pool action = do
  liftIO . withResource pool $ \conn -> action conn


withConn :: PG r m => (Connection -> IO a) -> m a
withConn g = do
  pool <- asks getter
  result <- liftIO $ Control.Exception.try $ withConn' pool g 
  case result of
    Right a -> return a
    Left (_ :: SomeException ) ->  throwError DataErrorPostgreSQLInServer
   

migrate :: State -> IO ()
migrate pool =
  withResource pool $ \conn -> do
    result <- withTransaction conn (runMigrations False conn cmds)
    case result of
      MigrationError err -> throwString err
      _ -> return ()
  where
    cmds =
      [ MigrationInitialization
      , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
      ]


requestForPost :: Query
requestForPost = [sql| select    endNews.id_news 
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
	 			               from (select * from news left join author on author.id_author = news.authors_id_news ) as endNews
                      |]


requestForPostFilter :: Query
requestForPostFilter = [sql|
                              select  distinct  endNews.id_news 
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
				                       from tags_news, (select * from news left join author on author.id_author = news.authors_id_news ) as endNews 
                        |]
  