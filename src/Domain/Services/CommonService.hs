module Domain.Services.CommonService where

import           ClassyPrelude                  ( Monad((>>), (>>=))
                                                , Int
                                                , Maybe
                                                , Text
                                                )
import           Domain.Types.ExportTypes       ( ErrorServer
                                                  ( EmptyQueryArray
                                                  , NotTakeEntity
                                                  )
                                                , SessionId
                                                , UserId
                                                , Quantity(Plural, One)
                                                , AnEntity
                                                , HelpForRequest
                                                  ( FilterNewsReq
                                                  , AuthorEntReq
                                                  , UserEntReq
                                                  , TagEntReq
                                                  , CommentEntReq
                                                  , CategoryEntReq
                                                  , DraftEntReq
                                                  , NewsEntReq
                                                  , SortedNewsReq
                                                  )
                                                )

import           Control.Monad.Except           ( MonadError(throwError) )
import           Domain.Services.Auth           ( Auth(findUserIdBySession) )
import           Domain.Services.AccessService  ( Access(..) )
import qualified Data.ByteString.Lazy.Internal as LB
import           Domain.Services.EntityService  ( Entity
                                                  ( getIntFromQueryArray
                                                  , fromAnEntity
                                                  , toQuantity
                                                  , toHelpForRequest
                                                  )
                                                )
import           Domain.Services.FilterService  ( filteredNews
                                                , FilterService
                                                )
import           Domain.Services.SortedOfService
                                                ( sortedNews
                                                , SortedOfService
                                                )

class (SortedOfService m, FilterService m) =>
      CommonService m
  where
  create :: AnEntity -> m  ()
  editing :: AnEntity -> m  ()
  editingAuthorAccess :: AnEntity -> UserId -> m  ()
  remove :: HelpForRequest -> Int -> m  ()
  removeAuthorAccess :: Int -> UserId ->  m  ()
  getAll :: HelpForRequest -> Int -> m  LB.ByteString
  getAllAuthorAccess :: UserId -> Int -> m LB.ByteString
  getOne :: HelpForRequest -> Int -> m  LB.ByteString
  getOneAuthorAccess :: Int -> UserId -> m  LB.ByteString
  publish :: UserId -> Int -> m  ()



publishAction :: CommonService m => SessionId -> Int -> m ()
publishAction sess idD = do
  idU <- findUserIdBySession sess
  publish idU idD

createCommon :: CommonService m => SessionId -> AnEntity -> m ()
createCommon sess ent = do
  helpR <- fromAnEntity ent
  case helpR of
    AuthorEntReq   -> checkAdminAccess sess >> create ent
    UserEntReq     -> create ent
    TagEntReq      -> checkAdminAccess sess >> create ent
    CommentEntReq  -> create ent
    CategoryEntReq -> checkAdminAccess sess >> create ent
    DraftEntReq    -> checkAuthorAccess sess >> create ent
    _              -> throwError NotTakeEntity

editingCommon :: CommonService m => SessionId -> AnEntity -> m ()
editingCommon sess ent = do
  helpR <- fromAnEntity ent
  case helpR of
    AuthorEntReq   -> checkAdminAccess sess >> editing ent
    UserEntReq     -> checkAdminAccess sess >> editing ent
    TagEntReq      -> checkAdminAccess sess >> editing ent
    CommentEntReq  -> editing ent
    CategoryEntReq -> checkAdminAccess sess >> editing ent
    DraftEntReq ->
      checkAuthorAccess sess
        >>  findUserIdBySession sess
        >>= editingAuthorAccess ent
    _ -> throwError NotTakeEntity

removeCommon :: CommonService m => SessionId -> HelpForRequest -> Int -> m ()
removeCommon sess helpReq idEnt = do
  case helpReq of
    AuthorEntReq   -> checkAdminAccess sess >> remove helpReq idEnt
    UserEntReq     -> checkAdminAccess sess >> remove helpReq idEnt
    TagEntReq      -> checkAdminAccess sess >> remove helpReq idEnt
    CommentEntReq  -> remove helpReq idEnt
    CategoryEntReq -> checkAdminAccess sess >> remove helpReq idEnt
    DraftEntReq ->
      checkAuthorAccess sess
        >>  findUserIdBySession sess
        >>= removeAuthorAccess idEnt
    NewsEntReq -> remove helpReq idEnt
    _          -> throwError NotTakeEntity

getOneCommon
  :: CommonService m => SessionId -> HelpForRequest -> Int -> m LB.ByteString
getOneCommon sess helpReq idE = do
  case helpReq of
    AuthorEntReq   -> checkAdminAccess sess >> getOne helpReq idE
    UserEntReq     -> getOne helpReq idE
    TagEntReq      -> getOne helpReq idE
    CommentEntReq  -> getOne helpReq idE
    CategoryEntReq -> getOne helpReq idE
    DraftEntReq ->
      checkAuthorAccess sess
        >>  findUserIdBySession sess
        >>= getOneAuthorAccess idE
    NewsEntReq -> getOne helpReq idE
    _          -> throwError NotTakeEntity

getArrayCommon
  :: CommonService m => SessionId -> HelpForRequest -> Int -> m LB.ByteString
getArrayCommon sess helpReq page = do
  case helpReq of
    AuthorEntReq   -> checkAdminAccess sess >> getAll helpReq page
    UserEntReq     -> checkAdminAccess sess >> getAll helpReq page
    TagEntReq      -> getAll helpReq page
    CommentEntReq  -> getAll helpReq page
    CategoryEntReq -> getAll helpReq page
    DraftEntReq    -> do
      checkAuthorAccess sess
      uId <- findUserIdBySession sess
      getAllAuthorAccess uId page
    NewsEntReq -> getAll helpReq page
    _          -> throwError NotTakeEntity

getCommon
  :: CommonService m
  => SessionId
  -> Text
  -> [(Text, Maybe Text)]
  -> m LB.ByteString
getCommon _    _           []  = throwError EmptyQueryArray
getCommon sess helpReqText arr = do
  quan <- toQuantity helpReqText
  case quan of
    One -> do
      help <- toHelpForRequest helpReqText
      page <- getIntFromQueryArray arr "id_Entity"
      getOneCommon sess help page
    Plural -> do
      help <- toHelpForRequest helpReqText
      case help of
        SortedNewsReq -> sortedNews arr
        FilterNewsReq -> do
          filteredNews arr
        _ -> do
          page <- getIntFromQueryArray arr "page"
          getArrayCommon sess help page

