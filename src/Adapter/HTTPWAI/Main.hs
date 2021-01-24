module Adapter.HTTPWAI.Main where

import ClassyPrelude
import qualified Network.Wai as W 
import qualified Network.Wai.Handler.Warp as W 
import Domain.Types.ImportTypes



--   getRequestBody :: W.Request -> m BC.ByteString
--   respond ::
--        HTTP.Status -> HTTP.ResponseHeaders -> BC.ByteString -> m W.Response

serverErrorResponse :: Monad m => ErrorServer ->  m W.Response
serverErrorResponse = undefined