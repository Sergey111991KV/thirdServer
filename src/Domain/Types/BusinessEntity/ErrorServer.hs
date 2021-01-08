module Domain.Types.BusinessEntity.ErrorServer ( errorText,
ErrorServer(ErrorConvert, ErrorTakeEntityNotSupposed, NotForAdmin,
            NotForAuthor, NotTakeEntity,DataErrorPostgreSQL,ErrorGetCookie,ErrorGetConfig))
where

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
  | NotAccessNotAdmin
  | ErrorTakeEntityNotSupposed
  | NotTakeEntity
  | DataErrorPostgreSQL
  | ErrorGetConfig
  | ErrorGetCookie
  | ErrorConvert
  | ErrorNonUserId
  | ErrorConvertNewsRaw
  deriving (Eq, Ord, Read, Show, Generic)

errorText :: ErrorServer -> Text
errorText err = pack $ show err
  