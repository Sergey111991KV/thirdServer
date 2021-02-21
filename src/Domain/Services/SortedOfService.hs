module Domain.Services.SortedOfService where

import           ClassyPrelude
import           Domain.Types.ExportTypes
import qualified Data.ByteString.Lazy.Internal as LB
import           Control.Monad.Except           ( MonadError(throwError) )
import           Domain.Services.AccessService
import           Domain.Services.EntityService


class Access m =>
  SortedOfService m where
  sortedDate ::  Int -> m LB.ByteString
  sortedAuthor ::  Int -> m  LB.ByteString
  sortedCategory ::  Int -> m  LB.ByteString
  sortedPhoto ::  Int -> m  LB.ByteString


sortedNews :: SortedOfService m => [(Text, Maybe Text)] -> m LB.ByteString
sortedNews arr = do
  page            <- getIntFromQueryArray arr "page"
  conditionSorted <- getTextFromQueryArray arr "conditionSorted"
  case conditionSorted of
    "date"     -> sortedDate page
    "author"   -> sortedAuthor page
    "category" -> sortedCategory page
    "photo"    -> sortedPhoto page
    _          -> throwError ErrorTakeEntityNotSupposed
