module Domain.Types.BusinessEntity.Tag
  ( Tag(Tag, idTag, nameTag)
  )
where

import           ClassyPrelude                  ( Applicative(pure)
                                                , Eq
                                                , Generic
                                                , Int
                                                , IsString(fromString)
                                                , Show
                                                , String
                                                , ($)
                                                , unpack
                                                )
import qualified Data.Attoparsec.ByteString.Char8
                                               as A
import           Database.PostgreSQL.Simple.FromField
                                                ( fromJSONField
                                                , FromField(..)
                                                )


import           Domain.Types.ImportLibrary     ( fromPGRow'
                                                , textContent
                                                , FromJSON
                                                , Action(Plain, Many, Escape)
                                                , ToJSON
                                                , intDec
                                                , toJSONField
                                                , FromRow
                                                , ToField(..)
                                                , ToRow
                                                )

import qualified Prelude                       as P

data Tag = Tag
  { idTag   :: Int
  , nameTag :: String
  }
  deriving (Eq, Show, Generic)

instance FromField Tag where
  fromField f mb = fromPGRow' parseTag f mb

instance FromRow Tag

instance FromJSON Tag

instance ToField Tag where
  toField (Tag idT text) = Many
    [ Plain "("
    , Plain (intDec idT)
    , Plain ","
    , Escape (fromString text)
    , Plain ")"
    ]

instance ToRow Tag

instance FromField [Tag] where
  fromField = fromJSONField

instance ToField [Tag] where
  toField = toJSONField

instance ToJSON Tag





parseTag :: A.Parser Tag
parseTag = do
  _        <- A.char '('
  idTag'   <- textContent
  _        <- A.char ','
  nameTag' <- textContent
  _        <- A.char ')'
  pure (Tag (P.read $ ClassyPrelude.unpack idTag') (unpack nameTag'))
