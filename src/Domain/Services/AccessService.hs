module Domain.Services.AccessService where

import Domain.Types.ExportTypes ( SessionId )
import Domain.Services.Auth ( Auth )


class Auth m => Access m where
  checkAuthorAccess :: SessionId -> m  ()
  checkAdminAccess :: SessionId -> m  ()