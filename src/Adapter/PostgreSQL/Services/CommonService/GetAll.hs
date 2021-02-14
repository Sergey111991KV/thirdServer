module Adapter.PostgreSQL.Services.CommonService.GetAll where

import Adapter.PostgreSQL.Common (PG, requestForPost, withConn)
import ClassyPrelude
    ( otherwise,
      ($),
      Eq((==)),
      Monad(return),
      Functor(fmap),
      IO,
      (<$>),
      (++),
      map )
import Database.PostgreSQL.Simple (query_)
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


getAll :: PG r m => HelpForRequest -> m  [AnEntity]
getAll helpEnt
  | helpEnt == AuthorEntReq = do
    let qAuthor = "SELECT * from author limit 20;"
    result <- withConn $ \conn -> query_ conn qAuthor :: IO [Author]
    case result of
      [] -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
      authorsArray -> do
        writeLogD "gett all author success!"
        return  $ fmap AnEntity authorsArray
  | helpEnt == UserEntReq = do
    let qUser = "SELECT * from usernews limit 20;"
    result <- withConn $ \conn -> query_ conn qUser :: IO [User]
    case result of
      [] -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
      users -> do
        writeLogD "gett all user success!"
        return $ fmap AnEntity users
  | helpEnt == TagEntReq = do
    let qTag = "SELECT * from tag limit 20;"
    result <- withConn $ \conn -> query_ conn qTag :: IO [Tag]
    case result of
      [] -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
      users -> do
        writeLogD "gett all Tag success!"
        return $ fmap AnEntity users
  | helpEnt == CategoryEntReq = do
    let qCat =
          "with recursive temp1 (id_category, parent_category, name_category) as ( \
                                    \ select t1.id_category, t1.parent_category, t1.name_category \
                                    \ from category t1  \
                                    \ union \
                                    \ select t2.id_category, t2.parent_category, t2.name_category \
                                    \ from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) \
                                    \ SELECT distinct id_category, name_category, parent_category from temp1 limit 20;"
    result <- withConn $ \conn -> query_ conn qCat :: IO [CategoryRaw]
    case result of
      [] -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
      cat -> do
        writeLogD "gett all Category success!"
        return $ AnEntity <$> convertCategoryRawArray cat
  | helpEnt == NewsEntReq = do
    let qDraft = requestForPost ++ " limit 20;"
    result <- withConn $ \conn -> query_ conn qDraft :: IO [NewsRaw]
    case result of
      [] -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
      news -> do
        writeLogD "gett all News success!"
        return $ AnEntity <$> map convertNewsRaw news
  | otherwise = do
    writeLogE (errorText ErrorTakeEntityNotSupposed)
    throwError ErrorTakeEntityNotSupposed
