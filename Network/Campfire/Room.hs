module Network.Campfire.Room where

import Network.Campfire.Common

-- | Request type for getting 'Room's visible to authenticated user. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/rooms.md#get-rooms>
-- for additional information.
data GetRooms = GetRooms
              deriving (Show, Eq)

-- | Request type for getting 'Room's in which authenticated user is present.
-- See
-- <https://github.com/37signals/campfire-api/blob/master/sections/rooms.md#get-rooms>
-- for additional information.
data GetPresence = GetPresence
                 deriving (Show, Eq)

-- | Request type for getting information about 'RoomId'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/rooms.md#get-room>
-- for additional information.
data GetRoom = GetRoom RoomId
             deriving (Show, Eq)

-- | Request type for joining 'RoomId'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/rooms.md#join-room>
-- for additional information.
data JoinRoom = JoinRoom RoomId
              deriving (Show, Eq)

-- | Request type for leaving 'RoomId'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/rooms.md#leave-room>
-- for additional information.
data LeaveRoom = LeaveRoom RoomId
               deriving (Show, Eq)

-- | Request type for updating name of 'RoomId' with specified 'Text'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/rooms.md#update-room>
-- for additional information.
data UpdateRoomName = UpdateRoomName RoomId Text
                    deriving (Show, Eq)

-- | Request type for updating topic of 'RoomId' with specified 'Text'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/rooms.md#update-room>
-- for additional information.
data UpdateRoomTopic = UpdateRoomTopic RoomId Text
                     deriving (Show, Eq)

-- | Request type for locking 'RoomId', preventing transcripts from being
-- logged and others from joining. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/rooms.md#lock-room>
-- for additional information.
data LockRoom = LockRoom RoomId
              deriving (Show, Eq)

-- | Request type for unlocking 'RoomId', allowing transcripts to be
-- logged and others to join. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/rooms.md#unlock-room>
-- for additional information.
data UnlockRoom = UnlockRoom RoomId
                deriving (Show, Eq)

instance Requestable GetRooms [Room] where
  respObjName _ = "rooms"
  reqToUrl GetRooms = "/rooms.json"

instance Requestable GetRoom Room where
  respObjName _ = "room"
  reqToUrl (GetRoom rId) = roomUrlFn $ show rId

instance Requestable GetPresence [Room] where
  respObjName _ = "rooms"
  reqToUrl GetPresence = "/presence.json"

instance Requestable JoinRoom () where
  parseResp = noResponse
  httpMethod _ = POST
  reqToUrl (JoinRoom rId) = roomUrlFn (show rId ++ "/join")

instance Requestable LeaveRoom () where
  parseResp = noResponse
  httpMethod _ = POST
  reqToUrl (LeaveRoom rId) = roomUrlFn (show rId ++ "/leave")

instance Requestable UpdateRoomName () where
  parseResp = noResponse
  httpMethod _ = PUT
  reqToUrl (UpdateRoomName rId _) = roomUrlFn $ show rId
  reqBody (UpdateRoomName _ n) =
    encode $ object ["room" .= object ["name" .= n]]

instance Requestable UpdateRoomTopic () where
  parseResp = noResponse
  httpMethod _ = PUT
  reqToUrl (UpdateRoomTopic rId _) = roomUrlFn $ show rId
  reqBody (UpdateRoomTopic _ t) =
    encode $ object ["room" .= object ["topic" .= t]]

instance Requestable LockRoom () where
  parseResp = noResponse
  httpMethod _ = POST
  reqToUrl (LockRoom rId) = roomUrlFn (show rId ++ "/lock")

instance Requestable UnlockRoom () where
  parseResp = noResponse
  httpMethod _ = POST
  reqToUrl (UnlockRoom rId) = roomUrlFn (show rId ++ "/unlock")

roomUrlFn :: String -> String
roomUrlFn s = "/room/" ++ s ++ ".json"
