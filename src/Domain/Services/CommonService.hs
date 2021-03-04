module Domain.Services.CommonService where

import           ClassyPrelude                 
import           Domain.Types.ExportTypes       

import           Control.Monad.Except           ( MonadError(throwError) )
import           Domain.Services.Auth           ( Auth(findUserIdBySession) )
import           Domain.Services.AccessService  ( Access(..) )
import qualified Data.ByteString.Lazy.Internal as LB
import           Domain.Services.EntityService  
                                                
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
  create :: AnEntity -> m ()
  editing :: AnEntity -> m  ()
  editingAuthorAccess :: AnEntity -> UserId -> m  ()
  remove :: HelpForRequest -> Int -> m  ()
  removeAuthorAccess :: Int -> UserId ->  m  ()
  getAll :: HelpForRequest -> Int -> m  LB.ByteString
  getAllAuthorAccess :: UserId -> Int -> m LB.ByteString
  getOne :: HelpForRequest -> Int -> m  LB.ByteString
  getOneAuthorAccess :: Int -> UserId -> m  LB.ByteString
  publish :: UserId -> Int -> m  ()


checkDraftWithAuthor :: CommonService m =>  SessionId ->  AnEntity -> m ()
checkDraftWithAuthor  sess (AnDraft draft) = do
  checkAuthorAccess sess
  idA <- getAuthorId sess 
  checkId idA (idAuthorDraft draft)
checkDraftWithAuthor _ _ = do throwError NotTakeEntity


checkId :: CommonService m => Int ->  Int -> m ()
checkId idA idD = do
   if idA == idD then return () else throwError NotSupposedAuthor


publishAction :: CommonService m => SessionId -> Int -> m ()
publishAction sess idDraftEnt = do
  idU <- findUserIdBySession sess
  publish idU idDraftEnt

createCommon :: CommonService m => SessionId -> AnEntity -> m ()
createCommon sess ent = do
  helpR <- fromAnEntity ent
  case helpR of
    AuthorEntReq   -> checkAdminAccess sess >> create ent
    UserEntReq     -> create ent
    TagEntReq      -> checkAdminAccess sess >> create ent
    CommentEntReq  -> create ent
    CategoryEntReq -> checkAdminAccess sess >> create ent
    DraftEntReq    -> checkDraftWithAuthor sess ent
                      >> create ent 
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
      checkDraftWithAuthor sess ent
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
    DraftEntReq -> do 
      checkAuthorAccess sess
      idU <-  findUserIdBySession sess
      removeAuthorAccess idEnt idU
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

