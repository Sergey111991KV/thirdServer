module Domain.DomainEntityLogic.DomainEntityLogic where


import Domain.Types.ExportTypes
    ( ErrorServer(ErrorGetPageQueryConvertText, ErrorConvert,
                  ErrorSupposedHelpRequest),
      Draft,
      Category,
      Tag,
      Comment,
      Author,
      User,
      News,
      Quantity(..),
      AnEntity(..),
      HelpForRequest(SortedNewsReq, AuthorEntReq, UserEntReq, NewsEntReq,
                     TagEntReq, CommentEntReq, CategoryEntReq, DraftEntReq,
                     FilterNewsReq) )
import ClassyPrelude
    ( ($),
      Monad(return),
      Int,
      Maybe(..),
      Either,
      Text,
      ByteString,
      String,
      (.),
      either,
      unpack,
      MonadIO,
      IsMap(lookup),
      LazySequence(fromStrict) )
import Control.Monad.Except
    ( MonadError(throwError) )
import Data.Aeson ( eitherDecode )
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
toAnEntity b AuthorEntReq = do
  either (\_ -> throwError ErrorConvert)
         (return . AnAuthor)
         (eitherDecode $ fromStrict b :: Either String Author)
toAnEntity b UserEntReq = do
  either (\_ -> throwError ErrorConvert)
         (return . AnUser)
         (eitherDecode $ fromStrict b :: Either String User)
toAnEntity b NewsEntReq = do
  either (\_ -> throwError ErrorConvert)
         (return . AnNews)
         (eitherDecode $ fromStrict b :: Either String News)
toAnEntity b CommentEntReq = do
  either (\_ -> throwError ErrorConvert)
         (return . AnComment)
         (eitherDecode $ fromStrict b :: Either String Comment)
toAnEntity b TagEntReq = do
  either (\_ -> throwError ErrorConvert)
         (return . AnTag)
         (eitherDecode $ fromStrict b :: Either String Tag)
toAnEntity b DraftEntReq = do
  either (\_ -> throwError ErrorConvert)
         (return . AnDraft)
         (eitherDecode $ fromStrict b :: Either String Draft)
toAnEntity b CategoryEntReq = do
  either (\_ -> throwError ErrorConvert)
         (return . AnCategory)
         (eitherDecode $ fromStrict b :: Either String Category)
toAnEntity _ _ = throwError ErrorConvert





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
