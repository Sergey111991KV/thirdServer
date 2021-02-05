module Domain.Services.FilterService where

import ClassyPrelude ( Int, Text ) 
import Domain.Types.ImportTypes ( News ) 

class FilterService m where
  filterOfData :: Text -> Text -> m  [News]
  filterAuthor :: Int -> m  [News]
  filterCategory :: Int -> m  [News]
  filterTag :: Int -> m  [News]
  filterOneOfTags :: Text -> m  [News]
  filterAllOfTags :: Text -> m  [News]
  filterName :: Text -> m  [News]
  filterContent :: Text -> m [News]
