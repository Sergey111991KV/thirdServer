module Domain.Services.CommonService where

import ClassyPrelude ( Monad((>>=), (>>)), Int ) 
import Domain.Types.ExportTypes
    ( ErrorServer(NotTakeEntity),
      HelpForRequest(DraftEntReq, AuthorEntReq, UserEntReq, TagEntReq,
                     CommentEntReq, CategoryEntReq),
      SessionId,
      UserId,
      AnEntity,
      fromAnEntity )
import Control.Monad.Except ( MonadError(throwError) )
import Domain.Services.Auth (Auth(findUserIdBySession))
import Domain.Services.AccessService ( Access(..) ) 

class (Access m) =>
      CommonService m
  where
  create :: AnEntity -> m  ()
  editing :: AnEntity -> m  ()
  editingAuthorAccess :: AnEntity -> UserId -> m  ()
  remove :: HelpForRequest -> Int -> m  ()
  removeAuthorAccess :: Int -> UserId ->  m  ()
  getAll :: HelpForRequest -> m  [AnEntity]
  getAllAuthorAccess :: UserId -> m [AnEntity]
  getOne :: HelpForRequest -> Int -> m  AnEntity
  getOneAuthorAccess :: Int -> UserId -> m  AnEntity
  publish :: UserId -> Int -> m  ()


publishAction :: CommonService m => SessionId -> Int -> m ()
publishAction sess idD = do
  idU <- findUserIdBySession sess
  publish idU idD

createCommon :: CommonService m => SessionId -> AnEntity -> m  ()
createCommon sess ent = do
  case fromAnEntity ent of
    AuthorEntReq -> checkAdminAccess sess >> create ent
    UserEntReq -> create  ent 
    TagEntReq -> checkAdminAccess sess >> create  ent 
    CommentEntReq -> create  ent 
    CategoryEntReq -> checkAdminAccess sess >> create  ent 
    DraftEntReq -> checkAuthorAccess sess >> create  ent 
    _ -> throwError NotTakeEntity 

editingCommon :: CommonService m => SessionId -> AnEntity -> m  ()
editingCommon sess ent = do
  case fromAnEntity ent of
    AuthorEntReq -> checkAdminAccess sess >> editing ent
    UserEntReq ->  checkAdminAccess sess >> editing  ent -- this logic is not good, because need user check, but it is option - I can remove it
    TagEntReq -> checkAdminAccess sess >> editing  ent 
    CommentEntReq -> editing  ent 
    CategoryEntReq -> checkAdminAccess sess >> editing  ent 
    DraftEntReq -> checkAuthorAccess sess >> findUserIdBySession sess >>= editingAuthorAccess  ent 
    _ -> throwError NotTakeEntity

removeCommon :: CommonService m =>  SessionId -> HelpForRequest -> Int -> m  ()
removeCommon sess helpReq idEnt = do
  case helpReq of
    AuthorEntReq -> checkAdminAccess sess >> remove helpReq idEnt
    UserEntReq ->  checkAdminAccess sess >> remove  helpReq idEnt 
    TagEntReq -> checkAdminAccess sess >> remove  helpReq idEnt 
    CommentEntReq -> remove  helpReq idEnt 
    CategoryEntReq -> checkAdminAccess sess >> remove  helpReq idEnt 
    DraftEntReq -> checkAuthorAccess sess >> findUserIdBySession sess >>=  removeAuthorAccess idEnt 
    _ ->  throwError NotTakeEntity

getOneCommon :: CommonService m =>  SessionId -> HelpForRequest -> Int -> m  AnEntity
getOneCommon sess helpReq idE = do
  case helpReq of
    AuthorEntReq -> checkAdminAccess sess >> getOne helpReq idE
    UserEntReq -> getOne  helpReq idE 
    TagEntReq ->  getOne  helpReq idE 
    CommentEntReq -> getOne  helpReq idE 
    CategoryEntReq ->  getOne  helpReq idE 
    DraftEntReq -> checkAuthorAccess sess >> findUserIdBySession sess >>=  getOneAuthorAccess idE 
    _ -> throwError NotTakeEntity

getArrayCommon :: CommonService m => SessionId -> HelpForRequest -> m [AnEntity]
getArrayCommon sess helpReq  = do
  case helpReq of
    AuthorEntReq -> checkAdminAccess sess >> getAll helpReq 
    UserEntReq ->  checkAdminAccess sess >> getAll  helpReq  
    TagEntReq ->  getAll  helpReq  
    CommentEntReq -> getAll  helpReq  
    CategoryEntReq ->  getAll  helpReq  
    DraftEntReq -> checkAuthorAccess sess >> findUserIdBySession sess >>=  getAllAuthorAccess  
    _ -> throwError NotTakeEntity
