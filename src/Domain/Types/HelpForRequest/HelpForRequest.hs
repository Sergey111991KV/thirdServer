module Domain.Types.HelpForRequest.HelpForRequest where

import ClassyPrelude 

data HelpForRequest
  = AuthorEntReq
  | UserEntReq
  | NewsEntReq
  | TagEntReq
  | CommentEntReq
  | CategoryEntReq
  | DraftEntReq
  | NotEntity
  deriving (Ord, Eq, Show, Generic)

-- convertTextToHelpRequest :: Text -> HelpForRequest
-- convertTextToHelpRequest txtEnt
--   | txtEnt == "author" = AuthorEntReq
--   | txtEnt == "category" = CategoryEntReq
--   | txtEnt == "user" = UserEntReq
--   | txtEnt == "news" = NewsEntReq
--   | txtEnt == "tag" = TagEntReq
--   | txtEnt == "comment" = CommentEntReq
--   | txtEnt == "draft" = DraftEntReq
--   | otherwise = NotEntity
