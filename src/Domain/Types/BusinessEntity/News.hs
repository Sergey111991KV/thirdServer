module Domain.Types.BusinessEntity.News
  ( News(comments)
  , NewsRaw
  , convertNewsRaw
  ) where

import           ClassyPrelude                  ( Applicative((<*>))
                                                , Eq
                                                , Generic
                                                , Int
                                                , Monad(return)
                                                , Show
                                                , Text
                                                , UTCTime
                                                , ($)
                                                , (<$>)
                                                , ap
                                                , head
                                                , impureNonNull
                                                )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(fromPGArray) )
import           Domain.Types.BusinessEntity.Author
                                                ( Author(Author) )
import           Domain.Types.BusinessEntity.Category
                                                ( Category
                                                , CategoryRaw
                                                , convertCategoryRawArray
                                                )
import           Domain.Types.BusinessEntity.Comment
                                                ( Comment )
import           Domain.Types.BusinessEntity.Draft
                                                ( )
import           Domain.Types.BusinessEntity.Tag
                                                ( Tag )
import           Domain.Types.ImportLibrary     ( FromJSON
                                                , FromRow(..)
                                                , ToJSON
                                                , ToRow
                                                , field
                                                )

data News = News
  { idNews            :: Int
  , dataCreateNews    :: UTCTime
  , authors           :: Author
  , category          :: Category
  , textNews          :: Text
  , mainPhotoUrlNews  :: Text
  , otherPhotoUrlNews :: PGArray Text
  , shortNameNews     :: Text
  , comments          :: PGArray Comment
  , tegs              :: PGArray Tag
  }
  deriving (Eq, Show, Generic)

data NewsRaw = NewsRaw
  { idNewsRaw            :: Int
  , dataCreateNewsRaw    :: UTCTime
  , authorsRaw           :: Author
  , categoryRaw          :: PGArray CategoryRaw
  , textNewsRaw          :: Text
  , mainPhotoUrlNewsRaw  :: Text
  , otherPhotoUrlNewsRaw :: PGArray Text
  , shortNameNewsRaw     :: Text
  , commentsRaw          :: PGArray Comment
  , tegsRaw              :: PGArray Tag
  }
  deriving (Eq, Show, Generic)

instance FromRow NewsRaw where
  fromRow =
    NewsRaw
      <$> field
      <*> field
      <*> (return Author `ap` field `ap` field `ap` field)
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

convertNewsRaw :: NewsRaw -> News
convertNewsRaw (NewsRaw isNR datNR authNR catNR textNR mainNR otherNR shortNR commNR tagNR)
  = News isNR datNR authNR convCat textNR mainNR otherNR shortNR commNR tagNR
 where
  convCat = head $ impureNonNull $ convertCategoryRawArray $ fromPGArray catNR

instance FromRow News

instance ToRow News

instance FromJSON News

instance ToJSON News
