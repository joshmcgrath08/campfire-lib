module Network.Campfire.Account where

import Network.Campfire.Common

-- | Request type for getting Campfire account information. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/account.md>
-- for additional information.
data GetAccount = GetAccount
                deriving (Show, Eq)

instance Requestable GetAccount Account where
  respObjName GetAccount = "account"
  reqToUrl GetAccount = "/account.json"
