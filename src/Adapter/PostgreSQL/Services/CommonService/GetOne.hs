module Adapter.PostgreSQL.Services.CommonService.GetOne where

import Adapter.PostgreSQL.Common (PG, requestForPost, withConn)
import ClassyPrelude
    ( ($),
      Monad(return),
      Int,
      IO,
      (++),
      print,
      null,
      head,
      impureNonNull )
import Domain.Services.LogMonad ( Log(writeLogE, writeLogD) ) 
import Domain.Types.ExportTypes
   
import Control.Monad.Except ( MonadError(throwError) ) 
import Adapter.PostgreSQL.ImportLibrary
import qualified Data.ByteString.Lazy.Internal as LB



getOne :: PG r m => HelpForRequest -> Int -> m LB.ByteString 
getOne helpR idE = do
  case helpR of
    AuthorEntReq -> do
      let qAuthor = "SELECT * from author where id_author=(?)"
      i <- withConn $ \conn -> query conn qAuthor [idE] :: IO [Author]
      case i of
        [x] -> do
          writeLogD "getOne Author success!"
          return $ encode  x
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    UserEntReq -> do
      let qUser = "SELECT * from usernews where id_user=(?)"
      i <- withConn $ \conn -> query conn qUser [idE] :: IO [User]
      case i of
        [x] -> do
          writeLogD "getOne User success!"
          return $ encode x
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    NewsEntReq -> do
      let qNews = requestForPost ++ "where endNews.id_news = (?);"
      i <- withConn $ \conn -> query conn qNews [idE] :: IO [NewsRaw]
      print i
      case i of
        [x] -> do
          writeLogD "getOne News success!"
          return $ encode $ convertNewsRaw x
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    TagEntReq -> do
      let qTag = "SELECT * from tag where id_tag=(?)"
      i <- withConn $ \conn -> query conn qTag [idE] :: IO [Tag]
      case i of
        [x] -> do
          writeLogD "getOne Tag success!"
          return $ encode x
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
    CommentEntReq -> do
      let qComment = "SELECT * from comment where id_comment=(?)"
      i <- withConn $ \conn -> query conn qComment [idE] :: IO [Comment]
      case i of
        [x] -> do
          writeLogD "getOne Comment success!"
          return $ encode x
        _ -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
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
          writeLogE (errorText ErrorConvert ++ " finalCategoryConvert")
          throwError DataErrorPostgreSQL
        else do
          let cat = head $ impureNonNull $ convertCategoryRawArray i
          return $ encode cat
    _ -> do
      writeLogE (errorText ErrorTakeEntityNotSupposed)
      throwError ErrorTakeEntityNotSupposed
