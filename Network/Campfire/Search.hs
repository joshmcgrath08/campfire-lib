module Network.Campfire.Search where

import Network.Campfire.Common

-- | Request type for searching messages for 'Text' term. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/search.md>
-- for additional information.
data Search = Search Text
            deriving (Show, Eq)

instance Requestable Search [Message] where
  respObjName _ = "messages"
  reqToUrl (Search t) = "/search?q=" ++ unpack t ++ "&format=json"
