module Domain.Types.BusinessEntity.Author
  ( Author(Author, description, idAuthor, idLinkUser)
  ) where

import           ClassyPrelude                  ( Applicative((<*>))
                                                , Eq
                                                , Generic
                                                , Int
                                                , Show
                                                , Text
                                                , (<$>)
                                                )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Domain.Types.AuthEntity.Auth   ( UserId )
import           Domain.Types.ImportLibrary     ( FromJSON
                                                , FromRow(..)
                                                , ToField(..)
                                                , ToJSON
                                                , ToRow(..)
                                                , field
                                                , toJSONField
                                                )

data Author = Author
  { idAuthor    :: Int
  , idLinkUser  :: UserId
  , description :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Author

instance ToJSON Author

instance FromField Author where
  fromField u = fromJSONField u

instance ToField Author where
  toField u = toJSONField u

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field

instance ToRow Author where
  toRow auth =
    [ toField (idAuthor auth)
    , toField (idLinkUser auth)
    , toField (description auth)
    ]
