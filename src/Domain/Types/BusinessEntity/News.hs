module Domain.Types.BusinessEntity.News
  ( News(comments)
  , NewsRaw
  , convertNewsRaw
  ) where

import           ClassyPrelude                
import           Database.PostgreSQL.Simple.Types
                                               
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
import           Domain.Types.ImportLibrary   
import           Database.PostgreSQL.Simple.FromField  

data News = News
  { idNews            :: Int
  , dataCreateNews    :: UTCTime
  , authors           :: Author
  , category          :: Category
  , textNews          :: Text
  , mainPhotoUrlNews  :: Text
  , otherPhotoUrlNews :: [Text]
  , shortNameNews     :: Text
  , comments          :: [Comment]
  , tags              :: [Tag]
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
      -- fromPGArray

convertNewsRaw :: NewsRaw -> News
convertNewsRaw (NewsRaw isNR datNR authNR catNR textNR mainNR otherNR shortNR commNR tagNR)
  = News isNR datNR authNR convCat textNR mainNR (fromPGArray otherNR) shortNR (fromPGArray commNR) (fromPGArray tagNR)
 where
  convCat = head $ impureNonNull $ convertCategoryRawArray $ fromPGArray catNR

instance FromRow News

instance ToRow News 


instance FromField [Text]



instance ToField [Text]


instance FromJSON News

instance ToJSON News 