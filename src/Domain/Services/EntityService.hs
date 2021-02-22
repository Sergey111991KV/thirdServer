module Domain.Services.EntityService where

import ClassyPrelude ( Int, Maybe, Text, ByteString )
import Domain.Types.ExportTypes
    ( ErrorServer, Quantity, AnEntity, HelpForRequest )
import           Control.Monad.Except           ( MonadError )

class (MonadError ErrorServer m) =>
      Entity m
  where
    fromAnEntity :: AnEntity -> m HelpForRequest
    toAnEntity :: ByteString -> HelpForRequest -> m AnEntity
    toHelpForRequest :: Text -> m HelpForRequest
    toQuantity :: Text -> m Quantity
    getIntFromQueryArray :: [(Text, Maybe Text)] -> Text -> m Int
    getTextFromQueryArray :: [(Text, Maybe Text)] -> Text -> m Text

