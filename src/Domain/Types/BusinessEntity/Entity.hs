{-# LANGUAGE GADTs #-}


module Domain.Types.BusinessEntity.Entity  where
import ClassyPrelude
import Domain.Types.BusinessEntity.Author (Author)
import Domain.Types.BusinessEntity.Category (Category)
import Domain.Types.BusinessEntity.Comment ( Comment ) 
import Domain.Types.BusinessEntity.Draft (Draft)
import Domain.Types.BusinessEntity.News (News)
import Domain.Types.BusinessEntity.Tag (Tag)
import Domain.Types.BusinessEntity.User (User)

import Domain.Types.HelpForRequest.HelpForRequest
    ( HelpForRequest(NewsEntReq, CategoryEntReq, UserEntReq,
                     AuthorEntReq, DraftEntReq, TagEntReq, CommentEntReq) ) 

data AnEntity   = forall a.
    Entity a => AnEntity a

class Entity a where
  getHelpRequest :: a -> HelpForRequest
  getData :: AnEntity -> a
  fromDataComment :: a -> Comment
  fromDataUser :: a -> User
  fromDataTag  :: a -> Tag
  fromDataDraft  :: a -> Draft
  fromDataAuthor  :: a -> Author
  fromDataCategory  :: a -> Category
  fromDataNews  :: a -> News

fromAnEntity :: AnEntity  -> HelpForRequest
fromAnEntity (AnEntity a) = getHelpRequest a

instance Entity Comment where
  getHelpRequest _ = CommentEntReq
  getData (AnEntity a) =  fromDataComment a 
  fromDataComment a =  a
  


instance Entity Tag where
  getHelpRequest _ = TagEntReq
  getData (AnEntity a) =  fromDataTag a 
  fromDataTag a =  a


instance Entity Draft where
  getHelpRequest _ = DraftEntReq
  getData (AnEntity a) =  fromDataDraft a 
  fromDataDraft a =  a


instance Entity Author where
  getHelpRequest _ = AuthorEntReq
  getData (AnEntity a) =  fromDataAuthor a 
  fromDataAuthor a =  a


instance Entity User where
  getHelpRequest _ = UserEntReq
  getData (AnEntity a) =  fromDataUser a 
  fromDataUser a =  a

instance Entity Category where
  getHelpRequest _ = CategoryEntReq
  getData (AnEntity a) =  fromDataCategory a 
  fromDataCategory a =  a


instance Entity News where
  getHelpRequest _ = NewsEntReq
  getData (AnEntity a) =  fromDataNews a 
  fromDataNews a =  a
