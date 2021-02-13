{-# LANGUAGE GADTs #-}

module Domain.Types.BusinessEntity.Entity  where

import ClassyPrelude
import Domain.Types.BusinessEntity.Author (Author)
import Domain.Types.BusinessEntity.Category (Category)
import Domain.Types.BusinessEntity.Comment 
import Domain.Types.BusinessEntity.Draft (Draft)
import Domain.Types.BusinessEntity.News (News)
import Domain.Types.BusinessEntity.Tag (Tag)
import Domain.Types.BusinessEntity.User (User)
import Domain.Types.HelpForRequest.HelpForRequest 
import qualified Data.Strict.Maybe as SM


data AnEntity   = forall a.
    Entity a => AnEntity a



class  Entity a where
  getHelpRequest :: a -> HelpForRequest
  getData :: AnEntity -> a
  fromDataComment :: a -> SM.Maybe Comment
  fromDataUser :: a -> SM.Maybe User
  fromDataTag  :: a -> SM.Maybe Tag
  fromDataDraft  :: a -> SM.Maybe Draft
  fromDataAuthor  :: a -> SM.Maybe Author
  fromDataCategory  :: a -> SM.Maybe Category
  fromDataNews  :: a -> SM.Maybe News

fromAnEntity :: AnEntity  -> HelpForRequest
fromAnEntity (AnEntity a) = getHelpRequest a

instance Entity Comment where
  getHelpRequest _ = CommentEntReq
  getData (AnEntity a) = SM.fromJust $  fromDataComment a 
  fromDataComment a = SM.Just a
  fromDataUser _ = SM.Nothing
  fromDataTag _ = SM.Nothing
  fromDataDraft _  = SM.Nothing
  fromDataAuthor _ = SM.Nothing
  fromDataCategory _ = SM.Nothing
  fromDataNews _  = SM.Nothing

instance Entity Tag where
  getHelpRequest _ = TagEntReq
  getData (AnEntity a) =  SM.fromJust $ fromDataTag a 
  fromDataComment _ = SM.Nothing
  fromDataUser _ = SM.Nothing
  fromDataTag a = SM.Just a
  fromDataDraft _  = SM.Nothing
  fromDataAuthor _ = SM.Nothing
  fromDataCategory _ = SM.Nothing
  fromDataNews _  = SM.Nothing


instance Entity Draft where
  getHelpRequest _ = DraftEntReq
  getData (AnEntity a) =  SM.fromJust $ fromDataDraft a 
  fromDataComment _ = SM.Nothing
  fromDataUser _ = SM.Nothing
  fromDataTag _ = SM.Nothing
  fromDataDraft a  = SM.Just a
  fromDataAuthor _ = SM.Nothing
  fromDataCategory _ = SM.Nothing
  fromDataNews _  = SM.Nothing


instance Entity Author where
  getHelpRequest _ = AuthorEntReq
  getData (AnEntity a) =  SM.fromJust $ fromDataAuthor a 
  fromDataComment _ = SM.Nothing
  fromDataUser _ = SM.Nothing
  fromDataTag _ = SM.Nothing
  fromDataDraft _  = SM.Nothing
  fromDataAuthor a = SM.Just a
  fromDataCategory _ = SM.Nothing
  fromDataNews _  = SM.Nothing


instance Entity User where
  getHelpRequest _ = UserEntReq
  getData (AnEntity a) =  SM.fromJust $ fromDataUser a 
  fromDataComment _ = SM.Nothing
  fromDataUser a = SM.Just a
  fromDataTag _ = SM.Nothing
  fromDataDraft _  = SM.Nothing
  fromDataAuthor _ = SM.Nothing
  fromDataCategory _ = SM.Nothing
  fromDataNews _  = SM.Nothing

instance Entity Category where
  getHelpRequest _ = CategoryEntReq
  getData (AnEntity a) =  SM.fromJust $ fromDataCategory a 
  fromDataComment _ = SM.Nothing
  fromDataUser _ = SM.Nothing
  fromDataTag _ = SM.Nothing
  fromDataDraft _  = SM.Nothing
  fromDataAuthor _ = SM.Nothing
  fromDataCategory a = SM.Just a
  fromDataNews _  = SM.Nothing


instance Entity News where
  getHelpRequest _ = NewsEntReq
  getData (AnEntity a) =  SM.fromJust $ fromDataNews a 
  fromDataComment _ = SM.Nothing
  fromDataUser _ = SM.Nothing
  fromDataTag _ = SM.Nothing
  fromDataDraft _  = SM.Nothing
  fromDataAuthor _ = SM.Nothing
  fromDataCategory _ = SM.Nothing
  fromDataNews a = SM.Just a
