module Domain.Services.SortedOfService where

import ClassyPrelude ( Text ) 
import Domain.Types.ImportTypes ( News ) 

class SortedOfService m where
  sortedNews :: Text -> m [News]
