module Domain.DomainEntityLogic.DomainEntityLogic where


import           Domain.Types.ExportTypes



import           ClassyPrelude                  ( ($)
                                                , Monad(return)
                                                , Int
                                                , Maybe(..)
                                                , Text
                                                , ByteString
                                                , (.)
                                                , either
                                                , unpack
                                                , MonadIO
                                                , IsMap(lookup)
                                                , LazySequence(fromStrict)
                                                )


import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Aeson                     ( FromJSON
                                                , eitherDecode
                                                )
import qualified Prelude


fromAnEntity :: MonadError ErrorServer m => AnEntity -> m HelpForRequest
fromAnEntity (AnAuthor   _) = return AuthorEntReq
fromAnEntity (AnUser     _) = return UserEntReq
fromAnEntity (AnNews     _) = return NewsEntReq
fromAnEntity (AnComment  _) = return CommentEntReq
fromAnEntity (AnTag      _) = return TagEntReq
fromAnEntity (AnDraft    _) = return DraftEntReq
fromAnEntity (AnCategory _) = return CategoryEntReq



toAnEntity
  :: MonadError ErrorServer m => ByteString -> HelpForRequest -> m AnEntity
toAnEntity b AuthorEntReq   = decodeFromBytestring AnAuthor b
toAnEntity b UserEntReq     = decodeFromBytestring AnUser b
toAnEntity b NewsEntReq     = decodeFromBytestring AnNews b
toAnEntity b CommentEntReq  = decodeFromBytestring AnComment b
toAnEntity b TagEntReq      = decodeFromBytestring AnTag b
toAnEntity b DraftEntReq    = decodeFromBytestring AnDraft b
toAnEntity b CategoryEntReq = decodeFromBytestring AnCategory b
toAnEntity _ _              = throwError ErrorConvert


decodeFromBytestring
  :: (FromJSON b, MonadError ErrorServer m) => (b -> a) -> ByteString -> m a
decodeFromBytestring anent =
  either (\_ -> throwError ErrorConvert) (return . anent)
    . eitherDecode
    . fromStrict


toHelpForRequest :: MonadError ErrorServer m => Text -> m HelpForRequest
toHelpForRequest text = do
  case text of
    "author"     -> return AuthorEntReq
    "user"       -> return UserEntReq
    "news"       -> return NewsEntReq
    "tag"        -> return TagEntReq
    "comment"    -> return CommentEntReq
    "category"   -> return CategoryEntReq
    "draft"      -> return DraftEntReq
    "authors"    -> return AuthorEntReq
    "users"      -> return UserEntReq
    "news_s"     -> return NewsEntReq
    "tags"       -> return TagEntReq
    "comments"   -> return CommentEntReq
    "categories" -> return CategoryEntReq
    "drafts"     -> return DraftEntReq
    "filterNews" -> return FilterNewsReq
    "sortedNews" -> return SortedNewsReq
    _            -> throwError ErrorSupposedHelpRequest


toQuantity :: MonadError ErrorServer m => Text -> m Quantity
toQuantity text = do
  case text of
    "author"     -> return One
    "user"       -> return One
    "news"       -> return One
    "tag"        -> return One
    "comment"    -> return One
    "category"   -> return One
    "draft"      -> return One
    "authors"    -> return Plural
    "users"      -> return Plural
    "news_s"     -> return Plural
    "tags"       -> return Plural
    "comments"   -> return Plural
    "categories" -> return Plural
    "drafts"     -> return Plural
    "filterNews" -> return Plural
    "sortedNews" -> return Plural
    _            -> throwError ErrorSupposedHelpRequest


getIntFromQueryArray
  :: (MonadError ErrorServer m, MonadIO m)
  => [(Text, Maybe Text)]
  -> Text
  -> m Int
getIntFromQueryArray arr param = do
  let maybeResultArray = lookup param arr
  case maybeResultArray of
    Nothing         -> throwError ErrorGetPageQueryConvertText
    Just maybeResul -> do
      case maybeResul of
        Nothing     -> throwError ErrorGetPageQueryConvertText
        Just result -> do
          let resultInt = Prelude.read $ unpack result :: Int
          return resultInt

getTextFromQueryArray
  :: (MonadError ErrorServer m, MonadIO m)
  => [(Text, Maybe Text)]
  -> Text
  -> m Text
getTextFromQueryArray arr param = do
  let maybeResultArray = lookup param arr
  case maybeResultArray of
    Nothing         -> throwError ErrorGetPageQueryConvertText
    Just maybeResul -> do
      case maybeResul of
        Nothing     -> throwError ErrorGetPageQueryConvertText
        Just result -> do
          return result
