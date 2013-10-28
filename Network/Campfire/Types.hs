-- | Types representing Campfire JSON types
{-# LANGUAGE TemplateHaskell #-}
module Network.Campfire.Types where

import Network.Campfire.Types.TH(deriveJSONOptions)

import Data.Time
import System.Locale
import Data.Text
import Data.Aeson
import Data.Aeson.TH

-- | Identifier for a 'User'.
type UserId = Integer

-- | Identifier for a 'Message'.
type MessageId = Integer

-- | Maximum number of participants allowed in a 'Room'.
type RoomLimit = Integer

-- | Identifier for a 'Room'.
type RoomId = Integer

-- | Identifier for an 'Upload'.
type UploadId = Integer

-- | Maximum size of an 'Upload' body in bytes.
type UploadSize = Integer

-- | Identifier for an 'Account'.
type AccountId = Integer

-- | Date/Time representation for created_at and updated_at
-- fields of data types.
newtype DateTime = DateTime UTCTime
                 deriving (Show, Eq)

instance FromJSON DateTime where
  parseJSON (String str) = do
    let pt = parseTime defaultTimeLocale formatString $ unpack str
    case pt of
      Nothing -> fail $ "Could not parse time: " ++ unpack str
      Just t -> return $ DateTime t
  parseJSON _ = fail "Expected string type, got other"

instance ToJSON DateTime where
  toJSON (DateTime lt) =
    String $ pack $ formatTime defaultTimeLocale formatString lt

formatString :: String
formatString = "%Y/%m/%d %T %Z"

-- | Type of user
data UserType = Member | Guest
              deriving (Show, Eq)

$(deriveJSON defaultOptions ''UserType)

-- | Datatype representing a user. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/data_reference.md#user>
-- for additional information.
data User = User
  { userId            :: !UserId
  , userName          :: !Text
  , userEmailAddress  :: !Text
  , userAdmin         :: !Bool
  , userCreatedAt     :: !DateTime
  , userType          :: !UserType
    -- | Not present in JSON if user avatar not set
  , userAvatar        :: !(Maybe Text)
  } deriving (Show, Eq)

$(deriveJSON deriveJSONOptions ''User)

-- | Type of 'Message'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/messages.md>
-- for additional information.
data MessageType = TextMessage
                 | PasteMessage
                 | SoundMessage
                 | AdvertisementMessage
                 | AllowGuestsMessage
                 | DisallowGuestsMessage
                 | IdleMessage
                 | KickMessage
                 | LeaveMessage
                 | EnterMessage
                 | SystemMessage
                 | TimestampMessage
                 | TopicChangeMessage
                 | UnidleMessage
                 | LockMessage
                 | UnlockMessage
                 | UploadMessage
                 | ConferenceCreatedMessage
                 | ConferenceFinishedMessage
                 deriving (Show, Eq)

$(deriveJSON defaultOptions ''MessageType)

-- | Datatype representing a message. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/data_reference.md#message>
-- for additional information.
data Message = Message
  { messageId        :: !MessageId
  , messageRoomId    :: !RoomId
    -- | Not present in 'TimeStamp' 'Message'
  , messageUserId    :: !(Maybe UserId)
    -- | Not present in 'LeaveMessage' 'Message'
  , messageBody      :: !(Maybe Text)
  , messageCreatedAt :: !DateTime
  , messageType      :: !MessageType
  , messageStarred   :: !Bool
  } deriving (Show, Eq)

$(deriveJSON deriveJSONOptions ''Message)

-- | Datatype representing a room. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/data_reference.md#room>
-- for additional information.
data Room = Room
  { roomId               :: !RoomId
  , roomName             :: !Text
  , roomTopic            :: !Text
    -- | Not present in 'GetRooms' response
  , roomLimit            :: !(Maybe RoomLimit)
    -- | Not present in 'GetRooms' response
  , roomFull             :: !(Maybe Bool)
    -- | Not present in 'GetRooms' response
  , roomOpenToGuests     :: !(Maybe Bool)
  , roomLocked           :: !Bool
    -- | Not present if room is not open to guests
  , roomActiveTokenValue :: !(Maybe Text)
  , roomUpdatedAt        :: !DateTime
  , roomCreatedAt        :: !DateTime
    -- | Not present in 'GetRooms' response
  , roomUsers            :: !(Maybe [User])
  } deriving (Show, Eq)

$(deriveJSON deriveJSONOptions ''Room)

-- | Datatype representing an upload. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/data_reference.md#upload>
-- for additional information.
data Upload = Upload
  { uploapId          :: !UploadId
  , uploadName        :: !Text
  , uploadRoomId      :: !RoomId
  , uploadUserId      :: !UserId
    -- | Not always present in 'Upload'
  , uploadSize        :: !(Maybe UploadSize)
  , uploadContentType :: !Text
  , uploadFullUrl     :: !Text
  , uploadCreatedAt   :: !DateTime
  } deriving (Show, Eq)

$(deriveJSON deriveJSONOptions ''Upload)

-- | Datatype representing a Campfire account and its owner. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/account.md>
-- for additional information
data Account = Account
  { accountId        :: !AccountId
  , accountName      :: !Text
  , accountSubdomain :: !Text
  , accountPlan      :: !Text
    -- | Account owner may not be present in 'GetAccount' response
  , accountOwner     :: !(Maybe Integer)
  , accounTimeZone   :: !Text
  , accountStorage   :: !Integer
  , accountCreatedAt :: !DateTime
  , accountUpdatedAt :: !DateTime
  } deriving (Show, Eq)

$(deriveJSON deriveJSONOptions ''Account)
