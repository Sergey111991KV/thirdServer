module Adapter.PostgreSQL.Services.FilterService where

import Adapter.PostgreSQL.Common
    ( requestForPost,
      withConn,
      PG,
      requestForPostFilter,
      requestForPostAllFilterTag )
import ClassyPrelude
import Domain.Services.LogMonad 
import Domain.Types.ExportTypes
import Adapter.PostgreSQL.ImportLibrary
import Control.Monad.Except ( MonadError(throwError) )
import qualified Prelude as   P

filterOfData :: PG r m => Text -> Text -> Int -> m [News]
filterOfData condition time page = do
  let q = requestForPost ++ conversCond condition ++ " limit 20 offset (?);"
  result <- withConn $ \conn -> query conn q (time,page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterOfData")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterOfData success "
      return $  map convertNewsRaw news

conversCond :: Text -> Query
conversCond txtCond 
                | txtCond ==  "less" =  " where data_creat_news <= (?)"
                | txtCond == "more" = " where data_creat_news >= (?)"
                | txtCond == "equel" = " where data_creat_news == (?)"
                | otherwise = "Error"

filterAuthor :: PG r m => Int -> Int -> m [News]
filterAuthor idA page = do
  let q = requestForPostFilter ++ " where endNews.id_author = (?) limit 20 offset (?);"
  result <- withConn $ \conn -> query conn q  (idA, page)   :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterAuthor")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterAuthor success "
      return $ map convertNewsRaw news

filterCategory :: PG r m => Int -> Int -> m [News]
filterCategory catId page = do
  let q = requestForPostFilter ++ " where endNews.category_id_news = (?) limit 20 offset (?);"
  result <- withConn $ \conn -> query conn q (catId, page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterCategory")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterCategory success "
      return $ map convertNewsRaw news

filterTag :: PG r m => Int -> Int -> m [News]
filterTag idT page = do
  let q = requestForPostFilter ++ " where tags_news.tags_id = (?) limit 20 offset (?);"
  liftIO $ P.print  idT
  result <- withConn $ \conn -> query conn q (idT, page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterTeg")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterTeg success "
      return $ map convertNewsRaw news

filterOneOfTags :: PG r m => Text -> Int -> m [News]
filterOneOfTags idTarray page = do
  let reqArr = "{" ++ idTarray ++ "}"
  let q = requestForPostFilter ++ " where  tags_news.tags_id = any (?) limit 20 offset (?);"
  result <-
    withConn $ \conn ->
      query conn q (reqArr, page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterOneOfTags")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterOneOfTags success "
      return $ map convertNewsRaw news

filterAllOfTags :: PG r m => Text -> Int -> m  [News]
filterAllOfTags idTarray page = do
  let reqArr = createAllTagRequest $ unpack idTarray
  let q = requestForPostAllFilterTag 
  result <-
    withConn $ \conn ->
      query conn q (reqArr, page) :: IO [NewsRaw]
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
 
filterName :: PG r m => Text -> Int -> m [News]
filterName txtName page = do
  let insertText = "%" ++ txtName ++ "%"
  let q = requestForPost ++ " where endNews.short_name_news LIKE (?) limit 20 offset (?);"
  result <- withConn $ \conn -> query conn q (insertText, page) :: IO [NewsRaw]
  case result of
    [] -> do
      writeLogE (errorText DataErrorPostgreSQL ++ " filterName")
      throwError DataErrorPostgreSQL
    news -> do
      writeLogD "filterName success "
      return $ map convertNewsRaw news

filterContent :: PG r m => Text -> Int -> m [News]
filterContent txtContent page = do
  let insertText = "%" ++ txtContent ++ "%"
  let q = requestForPost ++ " where endNews.text_news LIKE (?) limit 20 offset (?);"
  result <- withConn $ \conn -> query conn q (insertText, page) :: IO [NewsRaw]
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
