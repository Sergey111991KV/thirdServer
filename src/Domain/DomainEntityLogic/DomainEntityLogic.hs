module Domain.DomainEntityLogic.DomainEntityLogic  where


import Domain.Types.ExportTypes
import ClassyPrelude
import Control.Monad.Except 
import Data.Aeson 

fromAnEntity :: MonadError ErrorServer m =>  AnEntity -> m HelpForRequest
fromAnEntity (AnAuthor _ ) = return  AuthorEntReq
fromAnEntity (AnUser _) = return UserEntReq
fromAnEntity (AnNews _) = return NewsEntReq
fromAnEntity (AnComment _) = return CommentEntReq
fromAnEntity (AnTag _) = return TagEntReq
fromAnEntity (AnDraft _ ) = return DraftEntReq
fromAnEntity (AnCategory _) =  return CategoryEntReq 


toAnEntity :: MonadError ErrorServer m => ByteString -> HelpForRequest  -> m AnEntity
toAnEntity b AuthorEntReq = do
    either (\_ -> throwError ErrorConvert) (return . AnAuthor) (eitherDecode $ fromStrict b  :: Either String Author)
toAnEntity b UserEntReq = do
    either (\_ -> throwError ErrorConvert) (return . AnUser) (eitherDecode $ fromStrict b  :: Either String User)
toAnEntity b NewsEntReq = do
    either (\_ -> throwError ErrorConvert) (return . AnNews) (eitherDecode $ fromStrict b  :: Either String News)
toAnEntity b CommentEntReq = do
    either (\_ -> throwError ErrorConvert) (return . AnComment) (eitherDecode $ fromStrict b  :: Either String Comment)
toAnEntity b TagEntReq = do
    either (\_ -> throwError ErrorConvert) (return . AnTag) (eitherDecode $ fromStrict b  :: Either String Tag)
toAnEntity b DraftEntReq = do
    either (\_ -> throwError ErrorConvert) (return . AnDraft) (eitherDecode $ fromStrict b  :: Either String Draft)
toAnEntity b CategoryEntReq = do
    either (\_ -> throwError ErrorConvert) (return . AnCategory) (eitherDecode $ fromStrict b  :: Either String Category)
toAnEntity _ _ = throwError ErrorConvert


toHelpForRequest :: MonadError ErrorServer m => Text -> m HelpForRequest
toHelpForRequest text = do
    case text of
        "author" -> return  AuthorEntReq
        "user" -> return  UserEntReq
        "news" -> return  NewsEntReq
        "tag" -> return  TagEntReq
        "comment" -> return  CommentEntReq
        "category" -> return  CategoryEntReq
        "draft" -> return  DraftEntReq
        _ -> throwError ErrorSupposedHelpRequest

