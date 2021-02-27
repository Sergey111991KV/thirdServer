module Domain.Types.BusinessEntity.User
  ( User
    ( User
    , avatar
    , dataCreate
    , idUser
    , lastName
    , nameUser
    , userIsAdmin
    , userIsAuthor
    , userLogin
    , userPassword
    )
  )
where

import           ClassyPrelude                  ( Applicative((<*>))
                                                , Eq
                                                , Generic
                                                , Show
                                                , Text
                                                , UTCTime
                                                , (<$>)
                                                )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Domain.Types.AuthEntity.Auth   ( IsAdmin
                                                , IsAuthor
                                                , Login
                                                , Password
                                                , UserId
                                                )
import           Domain.Types.ImportLibrary     ( FromJSON
                                                , FromRow(..)
                                                , ToField(..)
                                                , ToJSON
                                                , ToRow(..)
                                                , field
                                                , toJSONField
                                                )

data User = User
  { idUser       :: UserId
  , nameUser     :: Text
  , lastName     :: Text
  , userLogin    :: Login
  , userPassword :: Password
  , avatar       :: Text
  , dataCreate   :: UTCTime
  , userIsAdmin  :: IsAdmin
  , userIsAuthor :: IsAuthor
  }
  deriving (Show, Eq, Generic)

instance FromJSON User

instance ToJSON User

instance FromField User where
  fromField u = fromJSONField u

instance ToField User where
  toField u = toJSONField u

instance FromRow User where
  fromRow =
    User
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

instance ToRow User where
  toRow u =
    [ toField (idUser u)
    , toField (nameUser u)
    , toField (lastName u)
    , toField (userLogin u)
    , toField (userPassword u)
    , toField (avatar u)
    , toField (dataCreate u)
    , toField (userIsAdmin u)
    , toField (userIsAuthor u)
    ]
