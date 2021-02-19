module Domain.Services.SortedOfService where

import           ClassyPrelude                  ( Int
                                                , Text
                                                )
import           Domain.Types.ExportTypes       ( News )

class SortedOfService m where
  sortedNews :: Text -> Int -> m [News]
