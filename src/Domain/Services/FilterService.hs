module Domain.Services.FilterService where

import ClassyPrelude ( Int, String ) 
import Domain.Types.ImportTypes ( News ) 

class FilterService m where
  filterOfData :: String -> String -> m  [News]
  filterAuthor :: Int -> m  [News]
  filterCategory :: Int -> m  [News]
  filterTeg :: Int -> m  [News]
  filterOneOfTags :: String -> m  [News]
  filterAllOfTags :: String -> m  [News]
  filterName :: String -> m  [News]
  filterContent :: String -> m [News]
