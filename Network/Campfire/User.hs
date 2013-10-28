module Network.Campfire.User where

import Network.Campfire.Common

-- | Request type for getting information about a 'UserId'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/users.md>
-- for additional information.
data GetUser = GetUser
  { -- | If 'Nothing', get information about current user.
    -- If 'Just', get information about 'UserId'.
    guUserId :: Maybe UserId
  }
             deriving (Show, Eq)

instance Requestable GetUser User where
  respObjName _ = "user"
  reqToUrl (GetUser mUId) = "/users/" ++ maybe "me" show mUId ++ ".json"
