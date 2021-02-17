module Adapter.PostgreSQL.Services.CommonService.GetAll where

import Adapter.PostgreSQL.Common (PG, requestForPost, withConn)
import ClassyPrelude
   
import Database.PostgreSQL.Simple 
import Domain.Services.LogMonad ( Log(writeLogE, writeLogD) ) 
import Domain.Types.ExportTypes
    ( errorText,
      ErrorServer(ErrorTakeEntityNotSupposed, DataErrorPostgreSQL),
      HelpForRequest(NewsEntReq, AuthorEntReq, UserEntReq, TagEntReq,
                     CategoryEntReq),
      convertCategoryRawArray,
      CategoryRaw,
      Tag,
      Author,
      User,
      convertNewsRaw,
      NewsRaw,
      AnEntity(..) )
import Control.Monad.Except ( MonadError(throwError) )


getAll :: PG r m => HelpForRequest -> Int -> m  [AnEntity]
getAll helpEnt p = do
  let page = p * 20
  case helpEnt of
    AuthorEntReq -> do
      let qAuthor = "SELECT * from author limit 20 offset (?);"
      result <- withConn $ \conn -> query conn qAuthor [page] :: IO [Author]
      case result of
        [] -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
        authorsArray -> do
          writeLogD "gett all author success!"
          return  $ fmap AnEntity authorsArray
    UserEntReq -> do
      let qUser = "SELECT * from usernews limit 20 offset (?);"
      result <- withConn $ \conn -> query conn qUser [page] :: IO [User]
      case result of
        [] -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
        users -> do
          writeLogD "gett all user success!"
          return $ fmap AnEntity users
    TagEntReq -> do
      let qTag = "SELECT * from tag limit 20 offset (?);"
      result <- withConn $ \conn -> query conn qTag [page] :: IO [Tag]
      case result of
        [] -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
        users -> do
          writeLogD "gett all Tag success!"
          return $ fmap AnEntity users
    CategoryEntReq -> do
      let qCat = "with recursive temp1 (id_category, parent_category, name_category) as ( \
                                    \ select t1.id_category, t1.parent_category, t1.name_category \
                                    \ from category t1  \
                                    \ union \
                                    \ select t2.id_category, t2.parent_category, t2.name_category \
                                    \ from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) \
                                    \ SELECT distinct id_category, name_category, parent_category from temp1 limit 20 offset (?);"
      result <- withConn $ \conn -> query conn qCat [page] :: IO [CategoryRaw]
      case result of
        [] -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
        cat -> do
          writeLogD "gett all Category success!"
          return $ AnEntity <$> convertCategoryRawArray cat
    NewsEntReq -> do
      let qDraft = requestForPost ++ " limit 20 offset (?);"
      result <- withConn $ \conn -> query conn qDraft [page] :: IO [NewsRaw]
      case result of
        [] -> do
          writeLogE (errorText DataErrorPostgreSQL)
          throwError DataErrorPostgreSQL
        news -> do
          writeLogD "gett all News success!"
          return $ AnEntity <$> map convertNewsRaw news
    _ -> do
      writeLogE (errorText ErrorTakeEntityNotSupposed)
      throwError ErrorTakeEntityNotSupposed
