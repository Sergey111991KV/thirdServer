module Domain.Services.AccessService where

import ClassyPrelude ( Int )
import Domain.Types.ExportTypes ( SessionId )      
import           Domain.Services.Auth           ( Auth )


class Auth m => Access m where
  checkAuthorAccess :: SessionId -> m  ()
  checkAdminAccess :: SessionId -> m  ()
  getAuthorId :: SessionId -> m  Int 
