module Domain.Types.BusinessEntity.ErrorServer where

import ClassyPrelude
    ( ($), Eq, Ord, Read, Show(show), Generic, Text, pack ) 

data ErrorServer
  = PasswordErrorInvalidAuth
  | LoginErrorInvalidAuth
  | NotCreateSession
  | NotGetUserIdFromSession
  | NotForAdmin
  | NotForAuthor
  | NotSupposedAuthor
  | NotAccessNotAdmid
  | NotAccessNotAuthor
  | ErrorTakeEntityNotSupposed
  | NotTakeEntity
  | DataErrorPostgreSQL
  | ErrorGetConfig
  | ErrorGetCookie
  | ErrorConvert
  | ErrorNonUserId
  | ErrorConvertNewsRaw
  | ErrorSupposedHelpRequest

  deriving (Eq, Ord, Read, Show, Generic)

errorText :: ErrorServer -> Text
errorText err = pack $ show err
  