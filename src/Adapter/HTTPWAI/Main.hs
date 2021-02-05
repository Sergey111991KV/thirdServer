module Adapter.HTTPWAI.Main where

import ClassyPrelude

import qualified Network.Wai.Handler.Warp as W 
import Domain.Types.ImportTypes
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP


--   getRequestBody :: W.Request -> m BC.ByteString
--   respond ::
--        HTTP.Status -> HTTP.ResponseHeaders -> BC.ByteString -> m W.Response

serverErrorResponse :: Monad m => ErrorServer ->  m HTTP.Response
serverErrorResponse err = do
        -- print errorText err
        pure $ HTTP.responseLBS HTTP.status404 [] (encodeUtf8 $ fromStrict $ errorText err)