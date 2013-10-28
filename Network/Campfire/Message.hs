module Network.Campfire.Message where

import Network.Campfire.Common

import Data.List(intercalate)
import Data.Maybe(isJust, fromJust)

-- | Request type for retrieving 'Message's. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/messages.md#get-recent-messages>
-- for additional information.
data GetMessages = GetMessages
  { -- | Get messages from 'RoomId'
    gmRoomID         :: RoomId
    -- | Limit for the number of messages to retrieve (default and max 100)
  , gmLimit          :: Maybe Integer
    -- | Only get messages after 'MessageID'
  , gmSinceMessageID :: Maybe MessageId
  }
                 deriving (Show, Eq)

-- | Request type for sending a 'Message'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/messages.md#create-message>
-- for additional information.
data SendMessage = SendMessage
  { -- | Send message to 'RoomId'
    smRoom        :: RoomId
    -- | Type of message to send
  , smMessageType :: MessageType
    -- | Body of message
  , smBody        :: Text
  }
                 deriving (Show, Eq)

-- | Request type for highlighting 'MessageId'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/messages.md#highlight-message>
-- for additional information.
data HighlightMessage = HighlightMessage MessageId
                      deriving (Show, Eq)

-- | Request type for un-highlighting 'MessageId'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/messages.md#unhighlight-message>
-- for additional information.
data UnhighlightMessage = UnhighlightMessage MessageId
                        deriving (Show, Eq)

-- | Request type for getting the transcript for 'RoomId'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/transcripts.md>
-- for additional information.
data GetTranscript = GetTranscript
  { -- | 'RoomId' specifies the room
    gtRoom :: RoomId
    -- | If 'Nothing', get transcript for today.
    -- If 'Just', get transcript for specified 'Month', 'Day', 'Year'.
  , gtDate :: (Maybe (Month, Day, Year))
  }
                   deriving (Show, Eq)

-- | Month representation for 'GetTranscript'.
type Month = Integer
-- | Day representation for 'GetTranscript'.
type Day = Integer
-- | Year representation for 'GetTranscript'.
type Year = Integer

instance Requestable GetMessages [Message] where
  respObjName _ = "messages"
  reqToUrl (GetMessages rId mLimit mId) =
    addQueryString
    [("limit", fmap show mLimit), ("since_message_id", fmap show mId)]
    (messageUrl rId "recent")

instance Requestable SendMessage Message where
  respObjName _ = "message"
  httpMethod _ = POST
  reqToUrl (SendMessage rId _ _) = messageUrl rId "speak"
  reqBody (SendMessage _ mType body) =
    encode $ object ["message" .= object
                     [ "type" .= show mType
                     , "body" .= body]]

instance Requestable HighlightMessage () where
  parseResp = noResponse
  httpMethod _ = POST
  reqToUrl (HighlightMessage mId) = highlightUrl mId

instance Requestable UnhighlightMessage () where
  parseResp = noResponse
  httpMethod _ = DELETE
  reqToUrl (UnhighlightMessage mId) = highlightUrl mId

instance Requestable GetTranscript [Message] where
  respObjName _ = "messages"
  reqToUrl (GetTranscript rId (Just (m, d, y))) =
    messageUrl rId $ concat ["transcript/", show y, "/", show m, "/", show d]
  reqToUrl (GetTranscript rId Nothing) = messageUrl rId "transcript"

messageUrl :: RoomId -> String -> String
messageUrl rId s = concat ["/room/", show rId, "/", s, ".json"]

highlightUrl :: MessageId -> String
highlightUrl mId = concat ["/messages/", show mId, "/star.json"]

addQueryString :: [(String, Maybe String)] -> String -> String
addQueryString opts url =
  let params = map (\(a,b) -> a ++ "=" ++ fromJust b)
               $ filter (isJust . snd) opts
  in url ++ if null params then "" else '?' : intercalate "&" params
