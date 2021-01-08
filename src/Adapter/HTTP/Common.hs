module Adapter.HTTP.Common where

import Blaze.ByteString.Builder (toLazyByteString)
import ClassyPrelude
  ( Bool(False, True)
  , Functor(fmap)
  , IsMap(lookup)
  , LazySequence(toStrict)
  , Maybe(..)
  , Monad(return)
  , MonadIO(..)
  , Num((+))
  , Text
  , Utf8(decodeUtf8, encodeUtf8)
  , ($)
  , (.)
  , (<$>)
  , getCurrentTime
  )
import Data.Aeson as Y (decode)
import qualified Data.ByteString.Lazy.Internal as BL
import Data.Time.Lens (modL, month)
import Domain.Types.ImportTypes as Y
 
import Web.Cookie
 
import Web.Scotty.Trans (ActionT, ScottyError, header, setHeader)

setCookie :: (ScottyError e, Monad m) => SetCookie -> ActionT e m ()
setCookie =
  setHeader "Set-Cookie" . decodeUtf8 . toLazyByteString . renderSetCookie

getCookie :: (ScottyError e, Monad m) => Text -> ActionT e m (Maybe Text)
getCookie key = do
  mCookieStr <- header "Cookie"
  return $ do
    cookie <- parseCookies . encodeUtf8 . toStrict <$> mCookieStr
    let bsKey = encodeUtf8 key
    val <- lookup bsKey cookie
    return $ decodeUtf8 val

defCookie :: SetCookie
defCookie = defaultSetCookie {setCookieName = "sId", setCookieValue = ""}

setDefaultCookie :: (ScottyError e, Monad m) => ActionT e m ()
setDefaultCookie = setCookie defCookie

setSessionIdInCookie ::
     (MonadIO m, ScottyError e) => SessionId -> ActionT e m ()
setSessionIdInCookie sId = do
  curTime <- liftIO getCurrentTime
  setCookie $
    def
      { setCookieName = "sId"
      , setCookiePath = Just "/"
      , setCookieValue = encodeUtf8 (sessionRaw sId)
      , setCookieExpires = Just $ modL month (+ 1) curTime
      , setCookieHttpOnly = True
      , setCookieSecure = False
      , setCookieSameSite = Just sameSiteLax
      }

funcDecodeToEntity :: Text -> BL.ByteString -> Maybe AnEntity
funcDecodeToEntity txtConvertEntity entBytestring = do
  case txtConvertEntity of
    "category" -> fmap AnEntity (decode entBytestring :: Maybe  Category)
    "author" -> fmap AnEntity (decode entBytestring :: Maybe  Author)
    "user" -> fmap AnEntity (decode entBytestring :: Maybe User)
    "tag" -> fmap AnEntity (decode entBytestring :: Maybe Tag)
    "draft" -> fmap AnEntity (decode entBytestring :: Maybe Draft)
    "comment" -> fmap AnEntity (decode entBytestring :: Maybe Comment)
    _ -> Nothing
