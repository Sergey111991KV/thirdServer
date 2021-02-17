module Adapter.HTTPWAI.Route where


import ClassyPrelude
import Control.Monad.Except
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as HTTP
import Domain.Types.ExportTypes
    ( ErrorServer(ErrorConvert, ErrorGetCookie),
      HelpForRequest(NewsEntReq, UserEntReq, AuthorEntReq,
                     CategoryEntReq, CommentEntReq, DraftEntReq, TagEntReq),
      Login(Login),
      Password(Password),
      Category,
      Draft,
      Tag,
      Author,
      User,
      Comment,
      News,
      AnEntity(AnEntity),
      Entity(getData) )
import Adapter.HTTPWAI.HelpFunction
    ( serverErrorResponse, successResponse, getCookie, setCookie )
import Domain.Services.ExportServices
    ( SortedOfService(..),
      sessionByAuth,
      FilterService(..),
      publishAction,
      CommonService,
      createCommon,
      editingCommon,
      removeCommon,
      getOneCommon,
      getArrayCommon,
      exitSession )
import Data.Aeson
import qualified BasicPrelude as BP

type Router = [Text] 

data API
  = POST Router
  | GET Router
  | PUT Router
  | DELETE Router
  | UNKNOWN
  deriving (Show, Eq)


route :: (Monad m, CommonService m, MonadIO m, SortedOfService m, FilterService m)
  => HTTP.Request -> m HTTP.Response
route req = do
  either  notAutorized  autorized (getCookie req)
  where
    notAutorized ErrorGetCookie = do
        case methodAndPath req of   
            GET  ["auth", login, pass] -> do
                newSess <- sessionByAuth (Login login) (Password pass)
                setCookie  newSess
            _ -> pure $ HTTP.responseLBS HTTP.status404 [] ""

    notAutorized err = serverErrorResponse err
        
    autorized sess = do 
        case methodAndPath req of   
            GET  ["auth","exit"] -> do
                exitSession sess
                return $ successResponse   ("auth exit" :: Text)

            GET  ["publish", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                publishAction sess unpackIdEntity
                return $ successResponse   ("publish news" :: Text)

            GET  ["news", "sortedNews", condition, page ] -> do
                let unpackPage = BP.read  page :: Int 
                news <- sortedNews condition unpackPage
                return $ successResponse news 

            GET  ["news", "filterAuthor", condition, page ] -> do
                let unpackPage = BP.read  page :: Int 
                let unpackIdAuthor = BP.read  condition :: Int 
                news <- filterAuthor unpackIdAuthor unpackPage
                return $ successResponse news 
            GET  ["news", "filterCategory", condition, page ] -> do
                let unpackPage = BP.read  page :: Int 
                let unpackIdCategory = BP.read  condition :: Int 
                news <- filterCategory unpackIdCategory unpackPage
                return $ successResponse news
            GET  ["news", "filterOfData", condition , date, page] -> do
                let unpackPage = BP.read  page :: Int 
                news <- filterOfData condition date unpackPage
                return $ successResponse news 
            GET  ["news", "filterOneOfTags", condition, page ] -> do
                let unpackPage = BP.read  page :: Int 
                news <- filterOneOfTags condition unpackPage
                return $ successResponse news 
            GET  ["news", "filterAllOfTags", condition, page ] -> do
                let unpackPage = BP.read  page :: Int 
                news <- filterAllOfTags condition unpackPage
                return $ successResponse news 
            GET  ["news", "filterName", condition, page ] -> do
                let unpackPage = BP.read  page :: Int 
                news <- filterName condition unpackPage
                return $ successResponse news 
            GET  ["news", "filterTag", condition, page ] -> do
                let unpackPage = BP.read  page :: Int 
                let unpackIdTag = BP.read  condition :: Int 
                news <- filterTag unpackIdTag unpackPage
                return $ successResponse news 
            GET  ["news", "filterContent", condition, page ] -> do
                let unpackPage = BP.read  page :: Int 
                news <- filterContent condition unpackPage
                return $ successResponse news 

            GET  ["user", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                user <- getOneCommon sess UserEntReq unpackIdEntity
                return $ successResponse  (getData  user :: User )
            GET  ["users",page] -> do
                let unpackPage = BP.read  page :: Int 
                users <- getArrayCommon sess UserEntReq unpackPage
                return $ successResponse  (map getData  users :: [User] )
            GET  ["author", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                author <- getOneCommon sess AuthorEntReq unpackIdEntity
                return $ successResponse  (getData  author :: Author )
            GET  ["authors",page] -> do
                let unpackPage = BP.read  page :: Int 
                authors <- getArrayCommon sess AuthorEntReq unpackPage
                return $ successResponse  (map getData  authors :: [Author] )
            GET  ["category", idE] -> do   
                let unpackIdEntity = BP.read  idE :: Int 
                category <- getOneCommon sess CategoryEntReq unpackIdEntity
                return $ successResponse  (getData  category :: Category )
            GET  ["categorys",page] -> do
                let unpackPage = BP.read  page :: Int 
                categorys <- getArrayCommon sess CategoryEntReq unpackPage
                return $ successResponse  (map getData  categorys :: [Category] )
            GET  ["comment", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                comment <- getOneCommon sess CommentEntReq unpackIdEntity
                return $ successResponse  (getData  comment :: Comment )
            GET  ["comments",page] -> do
                let unpackPage = BP.read  page :: Int 
                comments' <- getArrayCommon sess CommentEntReq unpackPage
                return $ successResponse  (map getData  comments' :: [Comment] )
            GET  ["draft", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                draft <- getOneCommon sess DraftEntReq unpackIdEntity
                return $ successResponse  (getData  draft :: Draft )
            GET  ["drafts", page] -> do
                let unpackPage = BP.read  page :: Int 
                drafts <- getArrayCommon sess DraftEntReq unpackPage
                return $ successResponse  (map getData  drafts :: [Draft] )
            GET  ["tag", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                tag <- getOneCommon sess TagEntReq unpackIdEntity
                return $ successResponse  (getData  tag :: Tag )
            GET  ["tags",page] -> do
                let unpackPage = BP.read  page :: Int 
                tags <- getArrayCommon sess TagEntReq unpackPage
                return $ successResponse  (map getData  tags :: [Tag] )
            GET  ["news", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                news <- getOneCommon sess NewsEntReq unpackIdEntity
                return $ successResponse  (getData  news :: News )
            GET  ["news_s",page] -> do
                let unpackPage = BP.read  page :: Int 
                news <- getArrayCommon sess NewsEntReq unpackPage
                return $ successResponse  (map getData  news :: [News] )

            POST  ["user"] -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (user :: User) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                createCommon sess (AnEntity user)
                return $ successResponse  ("success create user" :: Text)
            POST  ["author"] -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (author :: Author) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                createCommon sess (AnEntity author)
                return $ successResponse  ("success create author" :: Text)
            POST  ["category"] -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (category :: Category) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                createCommon sess (AnEntity category)
                return $ successResponse  ("success create category" :: Text)
            POST  ["comment"] -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (comment :: Comment) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                createCommon sess (AnEntity comment)
                return $ successResponse  ("success create comment" :: Text)
            POST  ["draft"] -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (category :: Draft) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                createCommon sess (AnEntity category)
                return $ successResponse  ("success create draft" :: Text)
            POST  ["tag"] -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (comment :: Tag) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                createCommon sess (AnEntity comment)
                return $ successResponse  ("success create tag" :: Text)

            PUT  ["user"] -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (user :: User) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                editingCommon sess (AnEntity user)
                return $ successResponse  ("success create user" :: Text)
            PUT  ["author"] -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (author :: Author) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                editingCommon sess (AnEntity author)
                return $ successResponse  ("success create author" :: Text)
            PUT  ["category"] -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (category :: Category) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                editingCommon sess (AnEntity category)
                return $ successResponse  ("success create category" :: Text)
            PUT  ["comment"] -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (comment :: Comment) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                editingCommon sess (AnEntity comment)
                return $ successResponse  ("success create comment" :: Text)
            PUT  ["draft"] -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (category :: Draft) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                editingCommon sess (AnEntity category)
                return $ successResponse  ("success create draft" :: Text)
            PUT  ["tag"] -> do
                reqBody <- liftIO $ HTTP.getRequestBodyChunk req
                (comment :: Tag) <- either (\_ -> throwError ErrorConvert) pure (eitherDecode $ fromStrict reqBody)
                editingCommon sess (AnEntity comment)
                return $ successResponse  ("success create tag" :: Text)

            DELETE  ["user", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                removeCommon sess UserEntReq unpackIdEntity
                return $ successResponse  ("delete create user" :: Text)
            DELETE  ["author", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                removeCommon sess AuthorEntReq unpackIdEntity
                return $ successResponse   ("delete create author" :: Text)
            DELETE  ["category", idE] -> do   
                let unpackIdEntity = BP.read  idE :: Int 
                removeCommon sess CategoryEntReq unpackIdEntity
                return $ successResponse  ("delete create category" :: Text)
            DELETE  ["comment", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                removeCommon sess CommentEntReq unpackIdEntity
                return $ successResponse  ("delete create comment" :: Text)
            DELETE  ["draft", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                removeCommon sess DraftEntReq unpackIdEntity
                return $ successResponse  ("delete create draft" :: Text)
            DELETE  ["tag", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                removeCommon sess TagEntReq unpackIdEntity
                return $ successResponse   ("delete create tag" :: Text)
            DELETE  ["news", idE] -> do
                let unpackIdEntity = BP.read  idE :: Int 
                removeCommon sess NewsEntReq unpackIdEntity
                return $ successResponse   ("delete create news" :: Text)

            _ -> pure $ HTTP.responseLBS HTTP.status404 [] ""


methodAndPath :: HTTP.Request -> API
methodAndPath req =
  case getMethod of
    HTTP.POST -> Adapter.HTTPWAI.Route.POST $ HTTP.pathInfo req
    HTTP.GET -> Adapter.HTTPWAI.Route.GET $ HTTP.pathInfo req
    HTTP.PUT -> Adapter.HTTPWAI.Route.PUT $ HTTP.pathInfo req
    HTTP.DELETE -> Adapter.HTTPWAI.Route.DELETE $ HTTP.pathInfo req
    _ -> UNKNOWN
  where
    getMethod =
      either (error . show) id $ HTTP.parseMethod (HTTP.requestMethod req)
