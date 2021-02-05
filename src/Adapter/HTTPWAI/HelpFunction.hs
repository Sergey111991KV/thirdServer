module Adapter.HTTPWAI.HelpFunction where

import ClassyPrelude

import Domain.Types.ImportTypes
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP
import Data.Aeson




serverErrorResponse :: Monad m => ErrorServer ->  m HTTP.Response
serverErrorResponse err = do
        pure $ HTTP.responseLBS HTTP.status404 [] (encodeUtf8 $ fromStrict $ errorText err)

successResponse :: forall a. ToJSON a  => a  -> HTTP.Response
successResponse  b = HTTP.responseBuilder HTTP.status200 [("Content-Type", "application/json")] $ fromEncoding $ toEncoding  b

getCookie :: HTTP.Request -> SessionId
getCookie _ = SessionId "OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v"

-- setCookie :: 

-- setDefaultCookie :: 