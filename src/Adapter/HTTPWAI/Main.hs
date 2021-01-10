module Adapter.HTTPWAI.Main where

import ClassyPrelude
import qualified Network.Wai as W 
import qualified Network.Wai.Handler.Warp as W 



class (Monad m) =>
      MonadHTTP m
  where

instance MonadHTTP IO where
--   getRequestBody :: W.Request -> m BC.ByteString
--   respond ::
--        HTTP.Status -> HTTP.ResponseHeaders -> BC.ByteString -> m W.Response

serverErrorResponse :: MonadHTTP m => m W.Response
serverErrorResponse = undefined