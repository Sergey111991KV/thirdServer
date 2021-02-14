module Domain.Services.SortedOfService where

import ClassyPrelude ( Text ) 
import Domain.Types.ExportTypes ( News ) 

class SortedOfService m where
  sortedNews :: Text -> m [News]
