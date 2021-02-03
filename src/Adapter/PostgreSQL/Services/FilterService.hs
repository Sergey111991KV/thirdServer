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
      Either(..),
      String,
      Text,
      MonadIO(liftIO),
      (++),
      map,
      pack )
import Adapter.PostgreSQL.ImportLibrary ( query, Query ) 
import Domain.Services.LogMonad (Log(writeLog))
import Domain.Types.ImportTypes
    ( LogLevel(Debug, ErrorLog),
      errorText,
      ErrorServer(DataErrorPostgreSQL),
      convertNewsRaw,
      News,
      NewsRaw )
import Control.Monad.Except ( MonadError(throwError) )
import qualified Prelude as   P

filterOfData :: PG r m => String -> String -> m  [News]
filterOfData condition time = do
  let q = requestForPost ++ conversCond condition
  result <- withConn $ \conn -> query conn q [time] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " filterOfData")
      throwError DataErrorPostgreSQL
    news -> do
      writeLog Debug "filterOfData success "
      return $  map convertNewsRaw news

conversCond :: String -> Query
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
      writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " filterAuthor")
      throwError DataErrorPostgreSQL
    news -> do
      writeLog Debug "filterAuthor success "
      return $ map convertNewsRaw news

filterCategory :: PG r m => Int -> m [News]
filterCategory catId = do
  let q = requestForPostFilterTag ++ " where endNews.category_id_news = (?) limit 20;"
  result <- withConn $ \conn -> query conn q [ catId ] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " filterCategory")
      throwError DataErrorPostgreSQL
    news -> do
      writeLog Debug "filterCategory success "
      return $ map convertNewsRaw news

filterTeg :: PG r m => Int -> m [News]
filterTeg idT = do
  let q = requestForPostFilterTag ++ " where tags_news.tags_id = (?) limit 20;"
  liftIO $ P.print  idT
  result <- withConn $ \conn -> query conn q [idT ] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " filterTeg")
      throwError DataErrorPostgreSQL
    news -> do
      writeLog Debug "filterTeg success "
      return $ map convertNewsRaw news

filterOneOfTags :: PG r m => String -> m [News]
filterOneOfTags idTarray = do
  let reqArr = "{" ++ idTarray ++ "}"
  let q = requestForPostFilterTag ++ " where  tags_news.tags_id = any (?) limit 20;"
  result <-
    withConn $ \conn ->
      query conn q [reqArr] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " filterOneOfTags")
      throwError DataErrorPostgreSQL
    news -> do
      writeLog Debug "filterOneOfTags success "
      return $ map convertNewsRaw news

filterAllOfTags :: PG r m => String -> m  [News]
filterAllOfTags idTarray = do
  let reqArr = createAllTagRequest idTarray
  let q = requestForPostAllFilterTag 
  result <-
    withConn $ \conn ->
      query conn q [reqArr] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " filterAllOfTags")
      throwError DataErrorPostgreSQL
    news -> do
      writeLog Debug "filterAllOfTags success "
      return $ map convertNewsRaw news

createAllTagRequest :: String -> String
createAllTagRequest  = createAllTagRequest' "%"  
  where
    createAllTagRequest' arr [] = arr
    createAllTagRequest' arr (',':xs) = createAllTagRequest' arr xs
    createAllTagRequest' arr (x:xs) = createAllTagRequest' (arr ++ [x] ++ "%") xs
 
filterName :: PG r m => String -> m  [News]
filterName txtName = do
  let insertText = "%" ++ txtName ++ "%"
  let q = requestForPost ++ " where endNews.short_name_news LIKE (?) limit 20;"
  result <- withConn $ \conn -> query conn q [insertText] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " filterName")
      throwError DataErrorPostgreSQL
    news -> do
      writeLog Debug "filterName success "
      return $ map convertNewsRaw news

filterContent :: PG r m => String -> m [News]
filterContent txtContent = do
  let insertText = "%" ++ txtContent ++ "%"
  let q = requestForPost ++ " where endNews.text_news LIKE (?) limit 20;"
  result <- withConn $ \conn -> query conn q [insertText] :: IO [NewsRaw]
  case result of
    [] -> do
      writeLog ErrorLog (errorText DataErrorPostgreSQL ++ " filterContent")
      throwError DataErrorPostgreSQL
    news -> do
      writeLog Debug "filterContent success "
      return $  map convertNewsRaw news

toStringFromArrayInt :: [Int] -> Text
toStringFromArrayInt array =
  pack $ "{" ++ P.foldl addParam "" array ++ "}"
  where
    addParam [] arr = show arr
    addParam elements arr = elements ++ (',' : show arr)
