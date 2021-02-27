module Domain.Services.FilterService where

import           ClassyPrelude                  ( Int
                                                , Maybe
                                                , Text
                                                )
import           Domain.Types.ExportTypes       ( ErrorServer
                                                  ( ErrorTakeEntityNotSupposed
                                                  )
                                                )
import qualified Data.ByteString.Lazy.Internal as LB
import           Control.Monad.Except           ( MonadError(throwError) )
import           Domain.Services.AccessService  ( Access )
import           Domain.Services.EntityService  ( Entity
                                                  ( getTextFromQueryArray
                                                  , getIntFromQueryArray
                                                  )
                                                )


class Access m =>
  FilterService m where
  filterOfData :: Text -> Text -> Int -> m LB.ByteString
  filterAuthor :: Int -> Int -> m LB.ByteString
  filterCategory :: Int -> Int -> m LB.ByteString
  filterTag :: Int -> Int -> m LB.ByteString
  filterOneOfTags :: Text -> Int -> m LB.ByteString
  filterAllOfTags :: Text -> Int -> m LB.ByteString
  filterName :: Text -> Int -> m LB.ByteString
  filterContent :: Text -> Int -> m LB.ByteString
  filterAllContent :: Text -> Int -> m LB.ByteString


filteredNews :: FilterService m => [(Text, Maybe Text)] -> m LB.ByteString
filteredNews arr = do
  page            <- getIntFromQueryArray arr "page"
  filterCondition <- getTextFromQueryArray arr "filterCondition"
  case filterCondition of
    "date" -> do
      condition <- getTextFromQueryArray arr "condition"
      date      <- getTextFromQueryArray arr "date"
      filterOfData condition date page
    "author" -> do
      idA <- getIntFromQueryArray arr "author_id"
      filterAuthor idA page
    "category" -> do
      idC <- getIntFromQueryArray arr "category_id"
      filterCategory idC page
    "tag" -> do
      idT <- getIntFromQueryArray arr "tag_id"
      filterTag idT page
    "oneOfTag" -> do
      arrTag <- getTextFromQueryArray arr "tags_arr"
      filterOneOfTags arrTag page
    "allOfTag" -> do
      arrTag <- getTextFromQueryArray arr "tags_arr"
      filterAllOfTags arrTag page
    "name" -> do
      name <- getTextFromQueryArray arr "name_filter"
      filterName name page
    "content" -> do
      content <- getTextFromQueryArray arr "content"
      filterContent content page
    "allContent" -> do
      content <- getTextFromQueryArray arr "content"
      filterAllContent content page
    _ -> throwError ErrorTakeEntityNotSupposed
