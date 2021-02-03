module Adapter.HTTPWAI.Route where

import ClassyPrelude
    ( ($),
      Eq,
      Monad(return),
      Show(show),
      Applicative(pure),
      error,
      Text,
      id,
      (.),
      either )
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP


import Adapter.HTTPWAI.Main

type Router = [Text] 



data API
  = POST Router
  | GET Router
  | PUT Router
  | DELETE Router
  | UNKNOWN
  deriving (Show, Eq)

route :: (Monad m)
  => HTTP.Request -> m HTTP.Response
route req = do
  case methodAndPath req of
    GET  [""] -> do
        
        return $  HTTP.responseLBS HTTP.status400  [] "Success"
    --   reqBody <- liftIO $ HTTP.getRequestBodyChunk req
    --   createUserRequest <-
    --     either fail pure (J.eitherDecode $ LBS.fromStrict reqBody)
    --   res <- createUserEndpoint createUserRequest
    --   pure $ HTTP.responseLBS HTTP.status200 [] "Success"
    GET  pass ->
      pure $ HTTP.responseLBS HTTP.status404 [] ""
    -- PUT (matches ["api", "user", ":pk"] -> Just [userId]) ->
    --   pure $ HTTP.responseLBS HTTP.status404 [] ""
    _ -> pure $ HTTP.responseLBS HTTP.status404 [] ""

methodAndPath :: HTTP.Request -> API
methodAndPath req =
  case getMethod of
    HTTP.POST -> POST $ HTTP.pathInfo req
    HTTP.GET -> GET $ HTTP.pathInfo req
    HTTP.PUT -> PUT $ HTTP.pathInfo req
    HTTP.DELETE -> DELETE $ HTTP.pathInfo req
    _ -> UNKNOWN
  where
    getMethod =
      either (error . show) id $ HTTP.parseMethod (HTTP.requestMethod req)

-- -- TODO refactor with fold
-- matches :: Path -> Path -> Maybe [Int]
-- matches path reqPath = checkRoute (T.unpack <$> path) (T.unpack <$> reqPath) []
--   where
--     checkRoute (x : xs) (y : ys) pks
--       | x == y = checkRoute xs ys pks
--       | x == ":pk" = checkRoute xs ys (read y : pks)
--       | otherwise = Nothing
--     checkRoute x y pks
--       | x == y = Just pks
--       | otherwise = Nothing