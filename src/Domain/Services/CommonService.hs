module Domain.Services.CommonService where

import ClassyPrelude ( Monad((>>=), (>>)), Int ) 
import Domain.Types.ImportTypes
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
  remove :: HelpForRequest -> Int -> m  ()
  removeDraft :: Int -> UserId ->  m  ()
  getAll :: HelpForRequest -> m  [AnEntity]
  getAllDraft :: UserId -> m [AnEntity]
  getOne :: HelpForRequest -> Int -> m  AnEntity
  getOneDraft :: Int -> UserId -> m  AnEntity
  publish :: UserId -> Int -> m  ()


publishAction :: CommonService m => SessionId -> Int -> m ()
publishAction sess idD = do
  idU <- findUserIdBySession sess
  publish idU idD

createCommon :: CommonService m => SessionId -> AnEntity -> m  ()
createCommon sess ent = do
  case fromAnEntity ent of
    AuthorEntReq -> checkAdminAccess sess >> create ent
    UserEntReq ->  checkAdminAccess sess >> create  ent 
    TagEntReq -> checkAdminAccess sess >> create  ent 
    CommentEntReq -> create  ent 
    CategoryEntReq -> checkAdminAccess sess >> create  ent 
    DraftEntReq -> checkAuthorAccess sess >> create  ent 
    _ -> throwError NotTakeEntity 

editingCommon :: CommonService m => SessionId -> AnEntity -> m  ()
editingCommon sess ent = do
  case fromAnEntity ent of
    AuthorEntReq -> checkAdminAccess sess >> editing ent
    UserEntReq ->  checkAdminAccess sess >> editing  ent 
    TagEntReq -> checkAdminAccess sess >> editing  ent 
    CommentEntReq -> editing  ent 
    CategoryEntReq -> checkAdminAccess sess >> editing  ent 
    DraftEntReq -> checkAuthorAccess sess >> editing  ent 
    _ -> throwError NotTakeEntity

removeCommon :: CommonService m =>  SessionId -> HelpForRequest -> Int -> m  ()
removeCommon sess helpReq idEnt = do
  case helpReq of
    AuthorEntReq -> checkAdminAccess sess >> remove helpReq idEnt
    UserEntReq ->  checkAdminAccess sess >> remove  helpReq idEnt 
    TagEntReq -> checkAdminAccess sess >> remove  helpReq idEnt 
    CommentEntReq -> remove  helpReq idEnt 
    CategoryEntReq -> checkAdminAccess sess >> remove  helpReq idEnt 
    DraftEntReq -> checkAuthorAccess sess >> findUserIdBySession sess >>=  removeDraft idEnt 
    _ ->  throwError NotTakeEntity

getOneCommon :: CommonService m =>  SessionId -> HelpForRequest -> Int -> m  AnEntity
getOneCommon sess helpReq idE = do
  case helpReq of
    AuthorEntReq -> checkAdminAccess sess >> getOne helpReq idE
    UserEntReq ->  checkAdminAccess sess >> getOne  helpReq idE 
    TagEntReq -> checkAdminAccess sess >> getOne  helpReq idE 
    CommentEntReq -> getOne  helpReq idE 
    CategoryEntReq -> checkAdminAccess sess >> getOne  helpReq idE 
    DraftEntReq -> checkAuthorAccess sess >> findUserIdBySession sess >>=  getOneDraft idE 
    _ -> throwError NotTakeEntity

geyArrayCommon :: CommonService m => SessionId -> HelpForRequest -> m [AnEntity]
geyArrayCommon sess helpReq  = do
  case helpReq of
    AuthorEntReq -> checkAdminAccess sess >> getAll helpReq 
    UserEntReq ->  checkAdminAccess sess >> getAll  helpReq  
    TagEntReq -> checkAdminAccess sess >> getAll  helpReq  
    CommentEntReq -> getAll  helpReq  
    CategoryEntReq -> checkAdminAccess sess >> getAll  helpReq  
    DraftEntReq -> checkAuthorAccess sess >> findUserIdBySession sess >>=  getAllDraft  
    _ -> throwError NotTakeEntity
