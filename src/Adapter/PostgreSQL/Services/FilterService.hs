module Adapter.PostgreSQL.Services.FilterService where

import Adapter.PostgreSQL.Common
    ( requestForPost,
      withConn,
      PG,
      requestForPostFilterTag,
      requestForPostAllFilterTag )
import ClassyPrelude
    ( otherwise,
      ($),
      Eq((==)),
      Monad(return),
      Show(show),
      Int,
      IO,
      String,
      Text,
      MonadIO(liftIO),
      (++),
      map,
      pack,
      unpack )
import Adapter.PostgreSQL.ImportLibrary ( query, Query ) 
import Domain.Services.LogMonad 
import Domain.Types.ImportTypes
    
import Control.Monad.Except ( MonadError(throwError) )
import qualified Prelude as   P

filterOfData :: PG r m => Text -> Text -> m  [News]
filterOfData condition time = do
  let q = requestForPost ++ conversCond condition
  result <- withConn $ \conn -> query conn q [time] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterOfData")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterOfData success "
      return $  map convertNewsRaw news

conversCond :: Text -> Query
conversCond txtCond 
                | txtCond ==  "less" =  " where data_creat_news <= (?);"
                | txtCond == "more" = " where data_creat_news >= (?);"
                | txtCond == "equel" = " where data_creat_news == (?);"
                | otherwise = "Error"

filterAuthor :: PG r m => Int -> m  [News]
filterAuthor idA = do
  let q = requestForPostFilterTag ++ " where endNews.id_author = (?) limit 20;"
  result <- withConn $ \conn -> query conn q  [idA]   :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterAuthor")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterAuthor success "
      return $ map convertNewsRaw news

filterCategory :: PG r m => Int -> m [News]
filterCategory catId = do
  let q = requestForPostFilterTag ++ " where endNews.category_id_news = (?) limit 20;"
  result <- withConn $ \conn -> query conn q [ catId ] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterCategory")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterCategory success "
      return $ map convertNewsRaw news

filterTag :: PG r m => Int -> m [News]
filterTag idT = do
  let q = requestForPostFilterTag ++ " where tags_news.tags_id = (?) limit 20;"
  liftIO $ P.print  idT
  result <- withConn $ \conn -> query conn q [idT ] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterTeg")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterTeg success "
      return $ map convertNewsRaw news

filterOneOfTags :: PG r m => Text -> m [News]
filterOneOfTags idTarray = do
  let reqArr = "{" ++ idTarray ++ "}"
  let q = requestForPostFilterTag ++ " where  tags_news.tags_id = any (?) limit 20;"
  result <-
    withConn $ \conn ->
      query conn q [reqArr] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterOneOfTags")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterOneOfTags success "
      return $ map convertNewsRaw news

filterAllOfTags :: PG r m => Text -> m  [News]
filterAllOfTags idTarray = do
  let reqArr = createAllTagRequest $ unpack idTarray
  let q = requestForPostAllFilterTag 
  result <-
    withConn $ \conn ->
      query conn q [reqArr] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterAllOfTags")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterAllOfTags success "
      return $ map convertNewsRaw news

createAllTagRequest :: String -> String
createAllTagRequest  = createAllTagRequest' "%"  
  where
    createAllTagRequest' arr [] = arr
    createAllTagRequest' arr (',':xs) = createAllTagRequest' arr xs
    createAllTagRequest' arr (x:xs) = createAllTagRequest' (arr ++ [x] ++ "%") xs
 
filterName :: PG r m => Text -> m  [News]
filterName txtName = do
  let insertText = "%" ++ txtName ++ "%"
  let q = requestForPost ++ " where endNews.short_name_news LIKE (?) limit 20;"
  result <- withConn $ \conn -> query conn q [insertText] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterName")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterName success "
      return $ map convertNewsRaw news

filterContent :: PG r m => Text -> m [News]
filterContent txtContent = do
  let insertText = "%" ++ txtContent ++ "%"
  let q = requestForPost ++ " where endNews.text_news LIKE (?) limit 20;"
  result <- withConn $ \conn -> query conn q [insertText] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterContent")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterContent success "
      return $  map convertNewsRaw news

toStringFromArrayInt :: [Int] -> Text
toStringFromArrayInt array =
  pack $ "{" ++ P.foldl addParam "" array ++ "}"
  where
    addParam [] arr = show arr
    addParam elements arr = elements ++ (',' : show arr)
