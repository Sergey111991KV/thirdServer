module Adapter.PostgreSQL.Services.CommonService.GetAll where

import Adapter.PostgreSQL.Common (PG, requestForPost, withConn)
import ClassyPrelude
   
  
import Database.PostgreSQL.Simple (query_)
import Domain.Services.LogMonad (Log(writeLog))
import Domain.Types.ImportTypes

getAll :: PG r m => HelpForRequest -> m (Either ErrorServer [AnEntity])
getAll helpEnt
  | helpEnt == AuthorEntReq = do
    let qAuthor = "SELECT * from author limit 20;"
    result <- withConn $ \conn -> query_ conn qAuthor :: IO [Author]
    case result of
      [] -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        return $ Left DataErrorPostgreSQL
      authorsArray -> do
        writeLog Debug "gett all author success!"
        return $ Right $ fmap AnEntity authorsArray
  | helpEnt == UserEntReq = do
    let qUser = "SELECT * from usernews limit 20;"
    result <- withConn $ \conn -> query_ conn qUser :: IO [User]
    case result of
      [] -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        return $ Left DataErrorPostgreSQL
      users -> do
        writeLog Debug "gett all user success!"
        return $ Right $ fmap AnEntity users
  | helpEnt == TagEntReq = do
    let qTag = "SELECT * from tag limit 20;"
    result <- withConn $ \conn -> query_ conn qTag :: IO [Tag]
    case result of
      [] -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        return $ Left DataErrorPostgreSQL
      users -> do
        writeLog Debug "gett all Tag success!"
        return $ Right $ fmap AnEntity users
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
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        return $ Left DataErrorPostgreSQL
      cat -> do
        writeLog Debug "gett all Category success!"
        return $ Right $ fmap AnEntity $ convertCategoryRawArray cat
  | helpEnt == NewsEntReq = do
    let qDraft = requestForPost ++ " limit 20;"
    result <- withConn $ \conn -> query_ conn qDraft :: IO [NewsRaw]
    case result of
      [] -> do
        writeLog ErrorLog (errorText DataErrorPostgreSQL)
        return $ Left DataErrorPostgreSQL
      news -> do
        writeLog Debug "gett all News success!"
        return $ Right $ fmap  AnEntity $ map convertNewsRaw news
  | otherwise = do
    writeLog ErrorLog (errorText ErrorTakeEntityNotSupposed)
    return $ Left ErrorTakeEntityNotSupposed
