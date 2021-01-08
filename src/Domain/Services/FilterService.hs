module Domain.Services.FilterService where

import ClassyPrelude ( Int, Either, String ) 
import Domain.Services.Auth (Auth)
import Domain.Types.ImportTypes (ErrorServer, News)

class (Auth m) =>
      FilterService m
  where
  filterOfData :: String -> String -> m (Either ErrorServer [News])
  filterAuthor :: Int -> m (Either ErrorServer [News])
  filterCategory :: Int -> m (Either ErrorServer [News])
  filterTeg :: Int -> m (Either ErrorServer [News])
  filterOneOfTags :: String -> m (Either ErrorServer [News])
  filterAllOfTags :: String -> m (Either ErrorServer [News])
  filterName :: String -> m (Either ErrorServer [News])
  filterContent :: String -> m (Either ErrorServer [News])
