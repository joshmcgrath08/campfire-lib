module Network.Campfire
       ( -- * Example Usage

         -- $example

         -- * Initializing the Library

         -- | The base URL and API token must be set before using
         -- this library. An example base URL is
         -- \"https:\/\/sample.campfirenow.com/\".
         -- Your API token can be found under the \"My Info\" tab in
         -- the Campfire web client.
         setBaseUrl
       , setApiToken
         -- * Making Requests
       , requestToResponse
       , Requestable
       , CompletedRequest(..)
         -- ** Account Requests
       , GetAccount(GetAccount)
         -- ** Message Requests
       , GetMessages(..)
       , SendMessage(..)
       , HighlightMessage(HighlightMessage)
       , UnhighlightMessage(UnhighlightMessage)
       , GetTranscript(..)
       , Month
       , Day
       , Year
         -- ** Room Requests
       , GetRooms(GetRooms)
       , GetPresence(GetPresence)
       , GetRoom(GetRoom)
       , JoinRoom(JoinRoom)
       , LeaveRoom(LeaveRoom)
       , UpdateRoomName(UpdateRoomName)
       , UpdateRoomTopic(UpdateRoomTopic)
       , LockRoom(LockRoom)
       , UnlockRoom(UnlockRoom)
         -- ** Search Requests
       , Search(Search)
         -- ** Upload Requests
       , GetFileUploads(GetFileUploads)
       , GetFileUpload(GetFileUpload)
       , UploadFile(UploadFile)
         -- ** User Requests
       , GetUser(..)
         -- * Datatypes representing Campfire data
       , User(..)
       , UserType(..)
       , UserId
       , Message(..)
       , MessageType(..)
       , MessageId
       , Room(..)
       , RoomLimit
       , RoomId
       , Upload(..)
       , UploadId
       , UploadSize
       , Account(..)
       , AccountId
       , DateTime(..)
       ) where

import Network.Campfire.Account
import Network.Campfire.Message
import Network.Campfire.Request
import Network.Campfire.Room
import Network.Campfire.Search
import Network.Campfire.Types
import Network.Campfire.Upload
import Network.Campfire.User

-- $example
-- @
-- import Network.Campfire
-- import System.Exit
-- import System.Environment
-- import qualified Data.Text as T
-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [url, apiToken] -> setBaseUrl url >> setApiToken apiToken
--     _ -> putStrLn \"Incorrect number of arguments\" >> exitFailure
--   rooms <- requestToResponse GetRooms
--   case rooms of
--     CompletedRequest ((Room { roomId = roomId }) : _) -> do
--       requestToResponse $ JoinRoom roomId
--       requestToResponse $
--         SendMessage roomId TextMessage $ T.pack \"This is a message\"
--       requestToResponse (GetMessages roomId Nothing Nothing) >>= print
--       requestToResponse $ LeaveRoom roomId
--       return ()
--     _ -> putStrLn \"Failed to get room\"
-- @
