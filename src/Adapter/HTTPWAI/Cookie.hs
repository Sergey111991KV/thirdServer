module Adapter.HTTPWAI.Cookie where




import ClassyPrelude
import Domain.Types.ImportTypes
import Control.Monad.Except
import Data.Aeson as Y (decode)
import qualified Data.ByteString.Lazy.Internal as BL
import Data.Time.Lens (modL, month)
import Domain.Types.ImportTypes as Y
 
import Web.Cookie
import  Network.Wai
import Network.HTTP.Types.Header


-- import Blaze.ByteString.Builder (toLazyByteString)



-- setCookie ::  SetCookie -> RequestHeaders
-- setCookie =
--   setHeader "Set-Cookie" . decodeUtf8 . toLazyByteString . renderSetCookie

-- defCookie :: SetCookie
-- defCookie = defaultSetCookie {setCookieName = "sId", setCookieValue = ""}

-- getCookie :: Text  -> Request -> IO Text
-- getCookie nameCook req = do
--    let head = requestHeaders req
--    return "ss"

-- getHeaders :: RequestHeaders -> ByteString 
-- getHeaders (_,_) = "aa"
-- getHeaders _ = "aaa"
