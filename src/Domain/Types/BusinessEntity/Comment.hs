module Domain.Types.BusinessEntity.Comment
  ( Comment
    ( Comment
    , dataCreateComments
    , newsIdComments
    , textComments
    , usersIdComments
    )
  ) where

import           ClassyPrelude                  ( Applicative(pure)
                                                , Eq
                                                , Generic
                                                , Int
                                                , Show
                                                , Text
                                                , UTCTime
                                                , ($)
                                                , unpack
                                                )

import           Domain.Types.ImportLibrary     ( FromJSON
                                                , FromRow
                                                , ToField(..)
                                                , ToJSON
                                                , ToRow
                                                , fromPGRow'
                                                , textContent
                                                , timeFromByteString
                                                , toJSONField
                                                )

import qualified Data.Attoparsec.ByteString.Char8
                                               as A
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(PGArray) )
import           Domain.Types.AuthEntity.Auth   ( UserId(UserId) )

import qualified Prelude                       as P

data Comment = Comment
  { idComment          :: Int
  , textComments       :: Text
  , dataCreateComments :: UTCTime
  , newsIdComments     :: Int
  , usersIdComments    :: UserId
  }
  deriving (Eq, Show, Generic)

instance FromField Comment where
  fromField f mb = fromPGRow' parseComment f mb

instance FromRow Comment

instance ToRow Comment

instance ToField Comment where
  toField = toJSONField

instance ToJSON Comment

instance FromJSON Comment

parseComment :: A.Parser Comment
parseComment = do
  _      <- A.char '('
  idC    <- textContent
  _      <- A.char ','
  text   <- textContent
  _      <- A.char ','
  dataC  <- textContent
  _      <- A.char ','
  newId  <- textContent
  _      <- A.char ','
  userId <- textContent
  _      <- A.char ')'
  pure
    (Comment (P.read $ ClassyPrelude.unpack idC)
             text
             (timeFromByteString dataC)
             (P.read $ ClassyPrelude.unpack newId)
             (UserId $ P.read $ ClassyPrelude.unpack userId)
    )

instance FromJSON (PGArray Comment)

instance ToJSON (PGArray Comment)

instance ToField [Comment] where

instance FromField [Comment]


deriving instance
         Generic (PGArray Comment) => Generic (PGArray Comment)
