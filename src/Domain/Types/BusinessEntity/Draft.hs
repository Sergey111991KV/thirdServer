{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain.Types.BusinessEntity.Draft where

import           ClassyPrelude                  ( Eq
                                                , Functor(fmap)
                                                , Ord
                                                , Show
                                                , Applicative((<*>))
                                                , Generic
                                                , Int
                                                , Maybe
                                                , Text
                                                , UTCTime
                                                , (<$>)
                                                )
import           Domain.Types.ImportLibrary     ( FromJSON
                                                , ToJSON
                                                , field
                                                , toJSONField
                                                , FromRow(..)
                                                , ToField(..)
                                                , ToRow
                                                )



import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(fromPGArray) )
import           Database.PostgreSQL.Simple.FromField
                                                ( fromJSONField
                                                , FromField(..)
                                                )

data Draft = Draft
  { idDraft         :: Int
  , textDraft       :: Text
  , dataCreateDraft :: UTCTime
  , newsIdDraft     :: Maybe Int
  , mainPhotoUrl    :: Text
  , shortNameDraft  :: Text
  , otherPhotoUrl   :: [Text]
  , tagsId          :: [Int]
  , idAuthorDraft   :: Int
  }
  deriving (Show, Generic, Ord, Eq)

instance FromRow Draft where
  fromRow =
    Draft
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> fmap fromPGArray field
      <*> fmap fromPGArray field
      <*> field

instance ToRow Draft

instance FromField [Text] where
  fromField = fromJSONField

instance FromField [Int] where
  fromField = fromJSONField

instance ToField  [Text] where
  toField = toJSONField

instance ToField  [Int] where
  toField = toJSONField

instance FromJSON Draft

instance ToJSON Draft
