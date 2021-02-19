{-# LANGUAGE QuasiQuotes #-}
module Adapter.PostgreSQL.Services.CommonService.Remove where

import Adapter.PostgreSQL.Common (PG, withConn)
import ClassyPrelude
   
import Control.Monad.Except ( MonadError(throwError) )
import Domain.Types.ExportTypes
import Adapter.PostgreSQL.ImportLibrary
import Domain.Services.LogMonad ( Log(writeLogE, writeLogD) ) 


remove :: PG r m => HelpForRequest -> Int -> m ()
remove ent idE
  | ent == AuthorEntReq = do
    let q = [sql| DELETE FROM author WHERE id_author = (?);|]
    liftIO $ print q
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLogD "delete author good!"
        return ()
      _ -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | ent == UserEntReq = do
    let q = [sql| DELETE FROM usernews WHERE id_user = (?);|]
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLogD "delete user good!"
        return ()
      _ -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | ent == NewsEntReq = do
    let q = [sql| DELETE FROM news WHERE id_news = (?);|]
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLogD "delete news good!"
        return ()
      _ -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | ent == TagEntReq = do
    let q = [sql| DELETE FROM tag WHERE id_tag = (?);|]
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLogD "delete tag good!"
        return ()
      _ -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | ent == CategoryEntReq = do
    let q = [sql| DELETE FROM category WHERE id_category = (?);|]
    result <- withConn $ \conn -> execute conn q [idE]
    case result of
      1 -> do
        writeLogD "delete category1 good!"
        return ()
      _ -> do
        writeLogE (errorText DataErrorPostgreSQL)
        throwError DataErrorPostgreSQL
  | otherwise = do
    writeLogE (errorText ErrorTakeEntityNotSupposed)
    throwError ErrorTakeEntityNotSupposed
