module Adapter.PostgreSQL.Services.CommonService.GetOne where

import Adapter.PostgreSQL.Common (PG, requestForPost, withConn)
import ClassyPrelude
  ( Either(..)
  , IO
  , Int
  , Monad(return)
  , ($)
  , (++)
  , head
  , impureNonNull
  , null
  , print
  )
import Database.PostgreSQL.Simple (query)
import Domain.Services.LogMonad (Log(writeLog))
import Domain.Types.ImportTypes
 

getOne :: PG r m => HelpForRequest -> Int -> m (Either ErrorServer AnEntity)
getOne helpR idE = do
  case helpR of
    AuthorEntReq -> do
      let qAuthor = "SELECT * from author where id_author=(?)"
      i <- withConn $ \conn -> query conn qAuthor [idE] :: IO [Author]
      case i of
        [x] -> do
          writeLog Debug "getOne Author success!"
          return $ Right $ AnEntity x
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    UserEntReq -> do
      let qUser = "SELECT * from usernews where id_user=(?)"
      i <- withConn $ \conn -> query conn qUser [idE] :: IO [User]
      case i of
        [x] -> do
          writeLog Debug "getOne User success!"
          return $ Right $ AnEntity x
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    NewsEntReq -> do
      let qNews = requestForPost ++ "where endNews.id_news = (?);"
      i <- withConn $ \conn -> query conn qNews [idE] :: IO [NewsRaw]
      print i
      case i of
        [x] -> do
          writeLog Debug "getOne News success!"
          return $ Right $ AnEntity $ convertNewsRaw x
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    TagEntReq -> do
      let qTag = "SELECT * from tag where id_tag=(?)"
      i <- withConn $ \conn -> query conn qTag [idE] :: IO [Tag]
      case i of
        [x] -> do
          writeLog Debug "getOne Tag success!"
          return $ Right $ AnEntity x
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    CommentEntReq -> do
      let qComment = "SELECT * from comment where id_comment=(?)"
      i <- withConn $ \conn -> query conn qComment [idE] :: IO [Comment]
      case i of
        [x] -> do
          writeLog Debug "getOne Comment success!"
          return $ Right $ AnEntity x
        _ -> do
          writeLog ErrorLog (errorText DataErrorPostgreSQL)
          return $ Left DataErrorPostgreSQL
    CategoryEntReq -> do
      let qCategory =
            "with recursive temp1 (id_category, parent_category, name_category) as ( \
                                    \ select t1.id_category, t1.parent_category, t1.name_category \
                                    \ from category t1 where t1.id_category = (?) \
                                    \ union \
                                    \ select t2.id_category, t2.parent_category, t2.name_category \
                                    \ from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) \
                                    \ SELECT distinct id_category, name_category, parent_category from temp1"
      i <- withConn $ \conn -> query conn qCategory [idE] :: IO [CategoryRaw]
      if null i
        then do
          writeLog ErrorLog (errorText ErrorConvert ++ " finalCategoryConvert")
          return $ Left DataErrorPostgreSQL
        else do
          let cat = head $ impureNonNull $ convertCategoryRawArray i
          return $ Right $ AnEntity cat
    _ -> do
      writeLog ErrorLog (errorText ErrorTakeEntityNotSupposed)
      return $ Left ErrorTakeEntityNotSupposed
