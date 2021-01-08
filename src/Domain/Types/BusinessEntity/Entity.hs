{-# LANGUAGE GADTs #-}

module Domain.Types.BusinessEntity.Entity  where

import ClassyPrelude
import Domain.Types.BusinessEntity.Author (Author)
import Domain.Types.BusinessEntity.Category (Category)
import Domain.Types.BusinessEntity.Comment (Comment)
import Domain.Types.BusinessEntity.Draft (Draft)
import Domain.Types.BusinessEntity.News (News)
import Domain.Types.BusinessEntity.Tag (Tag)
import Domain.Types.BusinessEntity.User (User)
import Domain.Types.ImportLibrary 
import Domain.Types.HelpForRequest.HelpForRequest 

data AnEntity = forall a.
    Entity a => AnEntity a



-- data Entity
--   = EntAuthor Author
--   | EntCategory Category
--   | EntComment Comment
--   | EntDraft Draft
--   | EntNews News
--   | EntUser User
--   | EntTag Tag
--   deriving (Eq, Show, Generic)

-- instance FromJSON Entity

-- instance ToJSON Entity

class Entity a where
  getHelpRequest :: a -> HelpForRequest
  getData :: AnEntity -> a


-- instance Entity AnEntity where 

  -- convertToEntity :: a -> AnEntity
  -- convertFromEntity :: AnEntity -> a
  -- convertToEntityArray :: [a] -> [AnEntity]
  -- convertFromEntityArray :: [AnEntity] -> [a]

instance Entity Comment where
  -- convertToEntity a = EntComment a
  -- convertFromEntity (EntComment a) = a
  -- convertToEntityArray [] = []
  -- convertToEntityArray (x:xs) = convertToEntity x : convertToEntityArray xs
  -- convertFromEntityArray [] = []
  -- convertFromEntityArray (x:xs) =
  --   convertFromEntity x : convertFromEntityArray xs

instance Entity Tag where
  -- convertToEntity a = EntTag a
  -- convertFromEntity (EntTag a) = a
  -- convertToEntityArray [] = []
  -- convertToEntityArray (x:xs) = convertToEntity x : convertToEntityArray xs
  -- convertFromEntityArray [] = []
  -- convertFromEntityArray (x:xs) =
  --   convertFromEntity x : convertFromEntityArray xs

instance Entity Draft where
  -- convertToEntity a = EntDraft a
  -- convertFromEntity (EntDraft a) = a
  -- convertToEntityArray [] = []
  -- convertToEntityArray (x:xs) = convertToEntity x : convertToEntityArray xs
  -- convertFromEntityArray [] = []
  -- convertFromEntityArray (x:xs) =
  --   convertFromEntity x : convertFromEntityArray xs

instance Entity Author where
  -- convertToEntity a = EntAuthor a
  -- convertFromEntity (EntAuthor a) = a
  -- convertToEntityArray [] = []
  -- convertToEntityArray (x:xs) = convertToEntity x : convertToEntityArray xs
  -- convertFromEntityArray [] = []
  -- convertFromEntityArray (x:xs) =
  --   convertFromEntity x : convertFromEntityArray xs

instance Entity User where
  -- convertToEntity a = EntUser a
  -- convertFromEntity (EntUser a) = a
  -- convertToEntityArray [] = []
  -- convertToEntityArray (x:xs) = convertToEntity x : convertToEntityArray xs
  -- convertFromEntityArray [] = []
  -- convertFromEntityArray (x:xs) =
  --   convertFromEntity x : convertFromEntityArray xs

instance Entity Category where
  -- convertToEntity a = EntCategory a
  -- convertFromEntity (EntCategory a) = a
  -- convertToEntityArray [] = []
  -- convertToEntityArray (x:xs) = convertToEntity x : convertToEntityArray xs
  -- convertFromEntityArray [] = []
  -- convertFromEntityArray (x:xs) =
  --   convertFromEntity x : convertFromEntityArray xs

instance Entity News where
  -- convertToEntity a = EntNews a
  -- convertFromEntity (EntNews a) = a
  -- convertToEntityArray [] = []
  -- convertToEntityArray (x:xs) = convertToEntity x : convertToEntityArray xs
  -- convertFromEntityArray [] = []
  -- convertFromEntityArray (x:xs) =
  --   convertFromEntity x : convertFromEntityArray xs
-- instance ConvertEntity Entity where
--   convertToEntity News{...} = EntNews News
--  блин здесь нужен синтаксический сахар чтобы не писать постоянно внутренние параметры
