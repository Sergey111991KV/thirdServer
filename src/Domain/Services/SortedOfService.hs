module Domain.Services.SortedOfService where

import ClassyPrelude (Either, Text)
import Domain.Types.ImportTypes (ErrorServer, News)

class SortedOfService m where
  sortedNews :: Text -> m (Either ErrorServer [News])
