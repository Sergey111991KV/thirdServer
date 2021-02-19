module Domain.Services.EntityService where

import           ClassyPrelude
import           Domain.Types.ExportTypes
import           Control.Monad.Except           ( MonadError )

class (MonadError ErrorServer m) =>
      Entity m
  where
    fromAnEntity :: AnEntity -> m HelpForRequest
    toAnEntity :: ByteString -> HelpForRequest -> m AnEntity
    toHelpForRequest :: Text -> m HelpForRequest
