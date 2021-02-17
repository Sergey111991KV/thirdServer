module Domain.Services.FilterService where

import ClassyPrelude ( Int, Text ) 
import Domain.Types.ExportTypes ( News ) 

class FilterService m where
  filterOfData :: Text -> Text -> Int -> m [News]
  filterAuthor :: Int -> Int -> m [News]
  filterCategory :: Int -> Int -> m [News]
  filterTag :: Int -> Int -> m [News]
  filterOneOfTags :: Text -> Int -> m [News]
  filterAllOfTags :: Text -> Int -> m [News]
  filterName :: Text -> Int -> m [News]
  filterContent :: Text -> Int -> m [News]
