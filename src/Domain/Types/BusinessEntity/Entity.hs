{-# LANGUAGE GADTs #-}

module Domain.Types.BusinessEntity.Entity where

import ClassyPrelude ( Eq, Ord, Show, Generic )
import Domain.Types.BusinessEntity.Author ( Author )
import           Domain.Types.BusinessEntity.Category
                                                ( Category )
import Domain.Types.BusinessEntity.Comment ( Comment )
import           Domain.Types.BusinessEntity.Draft
                                                ( Draft )
import           Domain.Types.BusinessEntity.News
                                                ( News )
import           Domain.Types.BusinessEntity.Tag
                                                ( Tag )
import Domain.Types.BusinessEntity.User ( User )

data HelpForRequest
  = AuthorEntReq
  | UserEntReq
  | NewsEntReq
  | TagEntReq
  | CommentEntReq
  | CategoryEntReq
  | DraftEntReq
  | SortedNewsReq
  | FilterNewsReq
  | NotEntity
  deriving (Ord, Eq, Show, Generic)

data AnEntity = AnAuthor Author
              | AnUser User
              | AnNews News
              | AnComment Comment
              | AnTag Tag
              | AnDraft Draft
              | AnCategory Category

data Quantity = One | Plural
