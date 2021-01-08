module Domain.Services.CommonService where

import ClassyPrelude
  ( Bool(..)
  , Either(..)
  , Int
  , Maybe(..)
  , Monad(return)
  , ($)
  , (++)
  )

import Domain.Types.ImportTypes

import Database.PostgreSQL.Simple.Types (PGArray(fromPGArray))
import Domain.Services.Auth (Auth(findUserIdBySession))
import Domain.Services.LogMonad (Log(writeLog))

class (Auth m) =>
      CommonService m
  where
  create :: AnEntity -> m (Either ErrorServer ())
  editing :: AnEntity -> m (Either ErrorServer ())
  remove :: HelpForRequest -> Int -> m (Either ErrorServer ())
  removeDraft :: HelpForRequest -> UserId -> Int -> m (Either ErrorServer ())
  getAll :: HelpForRequest -> m (Either ErrorServer [AnEntity])
  getAllDraft :: UserId -> m (Either ErrorServer [AnEntity])
  getOne :: HelpForRequest -> Int -> m (Either ErrorServer AnEntity)
  getOneDraft :: SessionId -> Int -> m (Either ErrorServer AnEntity)
  publish :: UserId -> Int -> m (Either ErrorServer ())
  checkAuthorAccess :: SessionId -> m (Either ErrorServer Bool)
  checkAdminAccess :: SessionId -> m (Either ErrorServer Bool)

publishAction ::
     CommonService m => SessionId -> Int -> m (Either ErrorServer ())
publishAction sess idD = do
  resultCheck <- findUserIdBySession sess
  case resultCheck of
    Left err -> do
      writeLog ErrorLog (errorText err)
      return $ Left err
    Right idU -> do
      writeLog Debug "Get aceess user for creat"
      publish idU idD

                            -- Create

-- createCommon ::
--      CommonService m => SessionId -> AnEntity -> m (Either ErrorServer ())
-- createCommon _ Nothing = return $ Left ErrorConvert
-- createCommon sess  = do
--   case ent of
--     (EntAuthor author) -> createAdminAccess sess (EntAuthor author)
--     (EntCategory cat) -> createAdminAccess sess (EntCategory cat)
--     (EntComment comment) -> createUserAccess sess (EntComment comment)
--     (EntDraft draft) -> createAuthorAccess sess (EntDraft draft)
--     (EntUser user) -> createAdminAccess sess (EntUser user)
--     (EntTag tag) -> createAdminAccess sess (EntTag tag)
--     (EntNews _) -> return $ Left NotTakeEntity

createAuthorAccess ::
     CommonService m => SessionId -> AnEntity -> m (Either ErrorServer ())
createAuthorAccess sess ent = do
  resultCheck <- checkAuthorAccess sess
  case resultCheck of
    Left err -> do
      writeLog ErrorLog (errorText err)
      return $ Left err
    Right False -> do
      writeLog Debug (errorText NotForAuthor)
      return $ Left NotForAuthor
    Right True -> do
      writeLog Debug "Get aceess author for creat"
      create ent

createAdminAccess ::
     CommonService m => SessionId -> AnEntity -> m (Either ErrorServer ())
createAdminAccess sess ent = do
  resultCheck <- checkAdminAccess sess
  case resultCheck of
    Left err -> do
      writeLog ErrorLog (errorText err)
      return $ Left err
    Right False -> do
      writeLog Debug (errorText NotForAdmin)
      return $ Left NotForAdmin
    Right True -> do
      writeLog Debug "Get aceess admin for creat"
      create ent

createUserAccess ::
     CommonService m => SessionId -> AnEntity -> m (Either ErrorServer ())
createUserAccess sess ent = do
  resultCheck <- findUserIdBySession sess
  case resultCheck of
    Left err -> do
      writeLog ErrorLog (errorText err)
      return $ Left err
    Right _ -> do
      writeLog Debug "Get aceess user for creat"
      create ent

                          -- Edit

-- editingCommon ::
--      CommonService m => SessionId -> Maybe Entity -> m (Either ErrorServer ())
-- editingCommon _ Nothing = return $ Left ErrorConvert
-- editingCommon sess (Just (EntAuthor author)) =
--   editingAdminAccess sess (EntAuthor author)
-- editingCommon sess (Just (EntCategory cat)) =
--   editingAdminAccess sess (EntCategory cat)
-- editingCommon sess (Just (EntComment comment)) =
--   editingUserAccess sess (EntComment comment)
-- editingCommon sess (Just (EntDraft draft)) =
--   editingAuthorAccess sess (EntDraft draft)
-- editingCommon sess (Just (EntUser user)) =
--   editingAdminAccess sess (EntUser user)
-- editingCommon sess (Just (EntTag tag)) = editingAdminAccess sess (EntTag tag)
-- editingCommon _ _ = return $ Left ErrorTakeEntityNotSupposed

-- editingAuthorAccess ::
--      CommonService m => SessionId -> Entity -> m (Either ErrorServer ())
-- editingAuthorAccess sess ent = do
--   resultCheck <- checkAuthorAccess sess
--   case resultCheck of
--     Left err -> do
--       writeLog ErrorLog (errorText err)
--       return $ Left err
--     Right False -> do
--       writeLog Debug (errorText NotForAuthor)
--       return $ Left NotForAuthor
--     Right True -> do
--       writeLog Debug "Get aceess author for creat"
--       editing ent

-- editingAdminAccess ::
--      CommonService m => SessionId -> Entity -> m (Either ErrorServer ())
-- editingAdminAccess sess ent = do
--   resultCheck <- checkAdminAccess sess
--   case resultCheck of
--     Left err -> do
--       writeLog ErrorLog (errorText err)
--       return $ Left err
--     Right False -> do
--       writeLog Debug (errorText NotForAdmin)
--       return $ Left NotForAdmin
--     Right True -> do
--       writeLog Debug "Get aceess admin for creat"
--       editing ent

-- editingUserAccess ::
--      CommonService m => SessionId -> Entity -> m (Either ErrorServer ())
-- editingUserAccess sess ent = do
--   resultCheck <- findUserIdBySession sess
--   case resultCheck of
--     Left err -> do
--       writeLog ErrorLog (errorText err)
--       return $ Left err
--     Right _ -> do
--       writeLog Debug "Get aceess user for creat"
--       editing ent

--                          -- Remove

-- removeCommon ::
--      CommonService m
--   => SessionId
--   -> HelpForRequest
--   -> Int
--   -> m (Either ErrorServer ())
-- removeCommon sess helpReq idE = do
--   case helpReq of
--     AuthorEntReq -> removeAdminAccess sess helpReq idE
--     UserEntReq -> removeAdminAccess sess helpReq idE
--     TagEntReq -> removeAdminAccess sess helpReq idE
--     CommentEntReq -> removeAuthorAccess sess helpReq idE
--     CategoryEntReq -> removeAdminAccess sess helpReq idE
--     NewsEntReq -> removeAdminAccess sess helpReq idE
--     DraftEntReq -> removeAuthorAccess sess helpReq idE
--     NotEntity -> return $ Left NotTakeEntity

-- removeAuthorAccess ::
--      CommonService m
--   => SessionId
--   -> HelpForRequest
--   -> Int
--   -> m (Either ErrorServer ())
-- removeAuthorAccess sess helpReq idE = do
--   resultUser <- findUserIdBySession sess
--   case resultUser of
--     Left err -> do
--       writeLog ErrorLog (errorText err)
--       return $ Left err
--     Right uId -> do
--       writeLog Debug "get UserId"
--       removeDraft helpReq uId idE

-- removeAdminAccess ::
--      CommonService m
--   => SessionId
--   -> HelpForRequest
--   -> Int
--   -> m (Either ErrorServer ())
-- removeAdminAccess sess helpReq idE = do
--   resultCheck <- checkAdminAccess sess
--   case resultCheck of
--     Left err -> do
--       writeLog ErrorLog (errorText err)
--       return $ Left err
--     Right False -> do
--       writeLog Debug (errorText NotForAdmin)
--       return $ Left NotForAdmin
--     Right True -> do
--       writeLog Debug "Get aceess admin for creat"
--       remove helpReq idE

--                        -- GetOne

-- getEntityCommon ::
--      CommonService m
--   => SessionId
--   -> HelpForRequest
--   -> Int
--   -> m (Either ErrorServer Entity)
-- getEntityCommon sess helpReq idE = do
--   case helpReq of
--     AuthorEntReq -> getEntityAdmin sess helpReq idE
--     UserEntReq -> getEntityUser sess helpReq idE
--     TagEntReq -> getEntityUser sess helpReq idE
--     CommentEntReq -> getEntityUser sess helpReq idE
--     CategoryEntReq -> getEntityUser sess helpReq idE
--     DraftEntReq -> getEntityAuthor sess idE
--     NewsEntReq -> getEntityUser sess helpReq idE
--     NotEntity -> return $ Left NotTakeEntity

-- getEntityAdmin ::
--      CommonService m
--   => SessionId
--   -> HelpForRequest
--   -> Int
--   -> m (Either ErrorServer Entity)
-- getEntityAdmin sess helpReq idE = do
--   resultCheck <- checkAdminAccess sess
--   case resultCheck of
--     Left err -> do
--       writeLog ErrorLog (errorText err)
--       return $ Left err
--     Right False -> do
--       writeLog Debug (errorText NotForAdmin)
--       return $ Left NotForAdmin
--     Right True -> do
--       writeLog Debug "Get aceess admin for creat"
--       getOne helpReq idE

-- getEntityAuthor ::
--      CommonService m => SessionId -> Int -> m (Either ErrorServer Entity)
-- getEntityAuthor = getOneDraft

-- getEntityUser ::
--      CommonService m
--   => SessionId
--   -> HelpForRequest
--   -> Int
--   -> m (Either ErrorServer Entity)
-- getEntityUser sess helpReq idE = do
--   resultCheck <- findUserIdBySession sess
--   case resultCheck of
--     Left err -> do
--       writeLog ErrorLog (errorText err)
--       return $ Left err
--     Right _ -> do
--       writeLog Debug "Get aceess user for get"
--       getOne helpReq idE

--                                    -- GetArray

-- getEntityArray ::
--      CommonService m
--   => SessionId
--   -> HelpForRequest
--   -> m (Either ErrorServer [Entity])
-- getEntityArray sess helpReq = do
--   case helpReq of
--     AuthorEntReq -> getEntityArrayAdmin sess helpReq
--     UserEntReq -> getEntityArrayUser sess helpReq
--     TagEntReq -> getEntityArrayUser sess helpReq
--     CategoryEntReq -> getEntityArrayUser sess helpReq
--     DraftEntReq -> getEntityArrayAuthor sess
--     NewsEntReq -> getEntityArrayUser sess helpReq
--     _ -> return $ Left ErrorTakeEntityNotSupposed

-- getEntityArrayAdmin ::
--      CommonService m
--   => SessionId
--   -> HelpForRequest
--   -> m (Either ErrorServer [Entity])
-- getEntityArrayAdmin sess helpReq = do
--   resultCheck <- checkAdminAccess sess
--   case resultCheck of
--     Left err -> do
--       writeLog ErrorLog (errorText err)
--       return $ Left err
--     Right False -> do
--       writeLog Debug (errorText NotForAdmin)
--       return $ Left NotForAdmin
--     Right True -> do
--       writeLog Debug "Get aceess admin for creat"
--       getAll helpReq

-- getEntityArrayUser ::
--      CommonService m
--   => SessionId
--   -> HelpForRequest
--   -> m (Either ErrorServer [Entity])
-- getEntityArrayUser sess helpReq = do
--   resultCheck <- findUserIdBySession sess
--   case resultCheck of
--     Left err -> do
--       writeLog ErrorLog (errorText err)
--       return $ Left err
--     Right _ -> do
--       writeLog Debug "Get aceess user for get"
--       getAll helpReq

-- getEntityArrayAuthor ::
--      CommonService m => SessionId -> m (Either ErrorServer [Entity])
-- getEntityArrayAuthor sess = do
--   checkResultUser <- findUserIdBySession sess
--   case checkResultUser of
--     Left err -> do
--       writeLog Debug (errorText err)
--       return $ Left err
--     Right uid -> do
--       writeLog Debug "Get aceess author "
--       getAllDraft uid

-- -- getEntityArrayComment ::
-- --      CommonService m => Int -> m (Either ErrorServer [Entity])
-- -- getEntityArrayComment idNewsForComment = do
-- --   resultNews <- getOne NewsEntReq idNewsForComment
-- --   case resultNews of
-- --     Left err -> do
-- --       writeLog ErrorLog (errorText err)
-- --       return $ Left err
-- --     Right (EntNews news) -> do
-- --       let com = fromPGArray $ comments news
-- --                                  -- как лучше записать? вычисление отдельно или сразу в возвращаемое значение?
-- --       return $ Right $ convertToEntityArray com
-- --     Right _ -> do
-- --       writeLog
-- --         ErrorLog
-- --         (errorText ErrorTakeEntityNotSupposed ++
-- --          " not right data from getEntityArrayComment")
-- --       return $ Left ErrorTakeEntityNotSupposed
