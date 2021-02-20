module Adapter.HTTPWAI.Route where


import           ClassyPrelude
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as HTTP
import qualified Prelude
import           Domain.Types.ExportTypes
import           Adapter.HTTPWAI.HelpFunction
import           Domain.Services.ExportServices
import qualified BasicPrelude                  as BP
import           Control.Monad.Except

type Router = [Text]

data API
  = POST Router
  | GET Router
  | PUT Router
  | DELETE Router
  | UNKNOWN
  deriving (Show, Eq)

getPage :: (Monad m, CommonService m, MonadIO m, SortedOfService m, FilterService m)
  => HTTP.Request
  -> m Int
getPage req = do
  print $ HTTP.queryToQueryText $ HTTP.queryString req
  let maybePageArray = lookup "page" $ HTTP.queryToQueryText $ HTTP.queryString req
  case maybePageArray of
    Nothing -> throwError ErrorGetPageQuery
    Just maybePage -> do 
      case maybePage of
        Nothing ->  throwError ErrorGetPageQueryConvertText
        Just page ->  return $ (BP.read page :: Int)


route
  :: (Monad m, CommonService m, MonadIO m, SortedOfService m, FilterService m)
  => HTTP.Request
  -> m HTTP.Response
route req = do
  either notAutorized autorized (getCookie req)
 where
  notAutorized ErrorGetCookie = do
    case methodAndPath req of
      GET ["auth", login, pass] -> do
        newSess <- sessionByAuth (Login login) (Password pass)
        setCookie newSess
      _ -> pure $ HTTP.responseLBS HTTP.status404 [] ""

  notAutorized err = serverErrorResponse err

  autorized sess = do
    case methodAndPath req of
      GET ["auth", "exit"] -> do
        exitSession sess
        return $ successResponse ("auth exit" :: Text)

      GET ["publish", idE] -> do
        let unpackIdEntity = BP.read idE :: Int
        publishAction sess unpackIdEntity
        return $ successResponse ("publish news" :: Text)

      GET arr -> do
        let help = Prelude.head arr
        news <- getCommon sess help (HTTP.queryToQueryText $ HTTP.queryString req)
        return $ successResponse' news


      -- GET ["news", "sortedNews", condition, page] -> do
      --   print $ lookup "page" $ HTTP.queryToQueryText $ HTTP.queryString req
      --   let unpackPage = BP.read page :: Int
      --   news <- sortedNews condition unpackPage
      --   return $ successResponse news

      -- GET ["news", "filterAuthor", condition] -> do
      --   page  <- getPage req
      --   let unpackIdAuthor = BP.read condition :: Int
      --   news <- filterAuthor unpackIdAuthor page
      --   return $ successResponse news
      -- GET ["news", "filterCategory", condition, page] -> do
      --   page  <- getPage req
      --   let unpackIdCategory = BP.read condition :: Int
      --   news <- filterCategory unpackIdCategory unpackPage
      --   return $ successResponse news
      -- GET ["news", "filterOfData", condition, date, page] -> do
      --   page  <- getPage req
      --   news <- filterOfData condition date unpackPage
      --   return $ successResponse news
      -- GET ["news", "filterOneOfTags", condition, page] -> do
      --   page  <- getPage req
      --   news <- filterOneOfTags condition unpackPage
      --   return $ successResponse news
      -- GET ["news", "filterAllOfTags", condition, page] -> do
      --   page  <- getPage req
      --   news <- filterAllOfTags condition unpackPage
      --   return $ successResponse news
      -- GET ["news", "filterName", condition, page] -> do
      --   page  <- getPage req
      --   news <- filterName condition unpackPage
      --   return $ successResponse news
      -- GET ["news", "filterTag", condition, page] -> do
      --   page  <- getPage req
      --   let unpackIdTag = BP.read condition :: Int
      --   news <- filterTag unpackIdTag unpackPage
      --   return $ successResponse news
      -- GET ["news", "filterContent", condition, page] -> do
      --   page  <- getPage req
      --   news <- filterContent condition unpackPage
      --   return $ successResponse news

      -- GET ["user", idE] -> do
      --   let unpackIdEntity = BP.read idE :: Int
      --   user <- getOneCommon sess UserEntReq unpackIdEntity
      --   return $ successResponse' user
      -- GET ["users", page] -> do
      --   page  <- getPage req
      --   users <- getArrayCommon sess UserEntReq unpackPage
      --   return $ successResponse' users
      -- GET ["author", idE] -> do
      --   let unpackIdEntity = BP.read idE :: Int
      --   author <- getOneCommon sess AuthorEntReq unpackIdEntity
      --   return $ successResponse' author
      -- GET ["authors", page] -> do
      --   page  <- getPage req
      --   authors <- getArrayCommon sess AuthorEntReq unpackPage
      --   return $ successResponse' authors
      -- GET ["category", idE] -> do
      --   let unpackIdEntity = BP.read idE :: Int
      --   category <- getOneCommon sess CategoryEntReq unpackIdEntity
      --   return $ successResponse' category
      -- GET ["categorys", page] -> do
      --   page  <- getPage req
      --   categorys <- getArrayCommon sess CategoryEntReq unpackPage
      --   return $ successResponse' categorys
      -- GET ["comment", idE] -> do
      --   let unpackIdEntity = BP.read idE :: Int
      --   comment <- getOneCommon sess CommentEntReq unpackIdEntity
      --   return $ successResponse' comment
      -- GET ["comments", page] -> do
      --   page  <- getPage req
      --   comments' <- getArrayCommon sess CommentEntReq unpackPage
      --   return $ successResponse' comments'
      -- GET ["draft", idE] -> do
      --   let unpackIdEntity = BP.read idE :: Int
      --   draft <- getOneCommon sess DraftEntReq unpackIdEntity
      --   return $ successResponse' draft
      -- GET ["drafts", page] -> do
      --   page  <- getPage req
      --   drafts <- getArrayCommon sess DraftEntReq unpackPage
      --   return $ successResponse' drafts
      -- GET ["tag", idE] -> do
      --   let unpackIdEntity = BP.read idE :: Int
      --   tag <- getOneCommon sess TagEntReq unpackIdEntity
      --   return $ successResponse' tag
      -- GET ["tags", page] -> do
      --   page  <- getPage req
      --   tags <- getArrayCommon sess TagEntReq unpackPage
      --   return $ successResponse' tags
      -- GET ["news", idE] -> do
      --   let unpackIdEntity = BP.read idE :: Int
      --   news <- getOneCommon sess NewsEntReq unpackIdEntity
      --   return $ successResponse' news
      -- GET ["news_s", page] -> do
      --   page  <- getPage req
      --   news <- getArrayCommon sess NewsEntReq unpackPage
      --   return $ successResponse' news

      POST arr -> do
        help    <- toHelpForRequest $ Prelude.head arr
        reqBody <- liftIO $ HTTP.getRequestBodyChunk req
        ent     <- toAnEntity reqBody help
        createCommon sess ent
        return . successResponse $ "success request " ++ Prelude.head arr

      PUT arr -> do
        help    <- toHelpForRequest $ Prelude.head arr
        reqBody <- liftIO $ HTTP.getRequestBodyChunk req
        ent     <- toAnEntity reqBody help
        editingCommon sess ent
        return . successResponse $ "editing request " ++ Prelude.head arr

      DELETE [helpReq, idE] -> do
        help <- toHelpForRequest helpReq
        let unpackIdEntity = BP.read idE :: Int
        removeCommon sess help unpackIdEntity
        return . successResponse $ "delete request" ++ helpReq
      _ -> pure $ HTTP.responseLBS HTTP.status404 [] ""


methodAndPath :: HTTP.Request -> API
methodAndPath req = case getMethod of
  HTTP.POST   -> Adapter.HTTPWAI.Route.POST $ HTTP.pathInfo req
  HTTP.GET    -> Adapter.HTTPWAI.Route.GET $ HTTP.pathInfo req
  HTTP.PUT    -> Adapter.HTTPWAI.Route.PUT $ HTTP.pathInfo req
  HTTP.DELETE -> Adapter.HTTPWAI.Route.DELETE $ HTTP.pathInfo req
  _           -> UNKNOWN
 where
  getMethod =
    either (error . show) id $ HTTP.parseMethod (HTTP.requestMethod req)
