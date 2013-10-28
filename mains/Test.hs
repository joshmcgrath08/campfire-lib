{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Network.Campfire.Test

import System.Exit
import System.Environment

import Control.Monad(liftM)
import System.FilePath.Posix(takeFileName)
import System.IO
import Test.HUnit
import Network.HTTP.Conduit
import qualified Data.Text as T
import Control.Exception

import Data.Maybe(fromJust)

  -- TO TEST

  -- ACCOUNT
  -- GetAccount

  -- MESSAGES
  -- GetMessages RoomId (Maybe Integer) (Mayber Integer)
  -- SendMessage RoomId MessageType Text
  -- HighlightMessage MessageId
  -- UnhighlightMessage MessageId
  -- GetTranscript RoomId (Maybe (Month, Day, Year))

  -- ROOMS
  -- GetRooms
  -- GetPresence
  -- GetRoom RoomId
  -- JoinRoom RoomId
  -- LeaveRoom RoomId
  -- UpdateRoomName RoomId Text
  -- UpdateRoomTopic RoomId Text
  -- LockRoom RoomId
  -- UnlockRoom RoomId

  -- SEARCH
  -- Search Text

  -- UPLOADS
  -- GetFileUploads RoomId
  -- GetFileUpload RoomId MessageId

  -- USER
  -- GetUser (Maybe RoomId)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [url, apiToken] -> setBaseUrl url >> setApiToken apiToken
    _ -> help

  -- account
  getResp GetAccount

  rooms <- getResp GetRooms
  assertBool "room list non-empty" $ not $ null rooms

  -- make sure we aren't in any rooms, then check presence
  let roomId' = roomId $ head rooms
  mapM_ (getResp . LeaveRoom . roomId) rooms
  getResp GetPresence >>= assertBool "presence room list empty" . null

  myUser <- getResp $ GetUser Nothing

  -- join room
  checkJoin roomId' $ userId myUser

  -- update room name
  checkUpdateName roomId' "new room name 1"
  checkUpdateName roomId' "new room name 2"

  -- update room topic
  checkUpdateTopic roomId' "new room topic 1"
  checkUpdateTopic roomId' "new room topic 2"
  checkUpdateTopic roomId' ""

  checkRoomLock roomId'
  checkRoomUnlock roomId'
  checkRoomLock roomId'
  checkRoomUnlock roomId'

  -- check presence again, this time we should be in a room
  getResp GetPresence >>= assertBool "presence list non-empty" . not . null

  -- check that we're still in the room
  checkInRoom roomId' $ userId myUser

  -- check GetUser on a user id
  uid <- liftM (userId . head . fromJust . roomUsers) $
         getResp $ GetRoom roomId'
  getResp $ GetUser $ Just uid

  -- get messages
  msgs <- getResp $ GetMessages roomId' Nothing Nothing
  let lastMId = messageId $ last msgs

  -- get messages with extra options
  getResp $ GetMessages roomId' Nothing Nothing
  getResp $ GetMessages roomId' Nothing $ Just lastMId
  getResp $ GetMessages roomId' (Just 3) Nothing

  -- send messages
  textMessageId <- checkMessageSend roomId' TextMessage "This is a text message"
  pasteMessageId <-
    checkMessageSend roomId' PasteMessage "This is a paste message"

  -- highlight messages
  checkHighlightMessage roomId' textMessageId
  checkUnhighlightMessage roomId' textMessageId

  -- get transcript for today
  transcript <- getResp $ GetTranscript roomId' Nothing
  assertBool "transcript not empty" $ not $ null transcript
  assertBool "transcript contains text message"
    (textMessageId `elem` map messageId transcript)
  assertBool "transcript contains paste message"
    (pasteMessageId `elem` map messageId transcript)

  -- search
  getResp (Search "text") >>= assertBool "text search" . not . null
  getResp (Search "paste") >>= assertBool "paste search" . not . null
  getResp (Search "flimflam") >>= assertBool "flimflam search" . null

  -- uploads
  getResp $ GetFileUploads roomId'
  checkFileUpload roomId' "file-name.fake.txt" "These are the fake file contents"

  -- leave room
  checkLeave roomId' $ userId myUser

  -- check presence again
  getResp GetPresence >>=
    assertBool "empty presence after leaving all rooms" . null

  return ()

logFile :: FilePath
logFile = "data.log"

numRetries :: Int
numRetries = 3

getResp :: (Requestable req resp, Show req, Show resp) => req -> IO resp
getResp = getResp' numRetries

getResp' :: (Requestable req resp, Show req, Show resp) => Int -> req -> IO resp
getResp' 0 req = do
  putStrLn $ "REQUEST: " ++ show req
  assertFailure $ "Failed after " ++ show numRetries ++ " retries"
  return undefined -- never executed
getResp' retries req = do
  unparsedResp <-
    (liftM Right (request req >>= liftM responseBody . withManager . httpLbs))
    `catch` (\(_ :: HttpException) -> liftM Left $ getResp' (retries - 1) req)
  case unparsedResp of
    Left _ -> error "How did this happen!"
    Right upr ->
      case parseResp req upr of
        Left e -> do
          putStrLn $ "REQUEST: " ++ show req
          assertFailure $ "Failed to parse (req, unparsedResp, error): "
            ++ show (req, upr, e)
          return undefined -- never executed
        Right parsedResp -> do
          appendFile logFile $ show (req, upr, parsedResp)
          appendFile logFile "\n"
          return parsedResp

help :: IO ()
help = putStrLn helpMsg >> exitFailure
  where helpMsg = "Usage: campfire-get-data <url> <api token>"

checkJoin :: RoomId -> UserId -> IO ()
checkJoin roomId' userId' = do
  getResp $ JoinRoom roomId'
  checkInRoom roomId' userId'

checkLeave :: RoomId -> UserId -> IO ()
checkLeave roomId' userId' = do
  getResp $ LeaveRoom roomId'
  checkNotInRoom roomId' userId'

checkInRoom :: RoomId -> UserId -> IO ()
checkInRoom roomId' userId' = do
  room <- getResp $ GetRoom roomId'
  assertBool "we're in the room we joined" $
    userId' `elem` (map userId $ fromJust $ roomUsers room)

checkNotInRoom :: RoomId -> UserId -> IO ()
checkNotInRoom roomId' userId' = do
  room <- getResp $ GetRoom roomId'
  assertBool "we're not in the room we left" $
    userId' `notElem` (map userId $ fromJust $ roomUsers room)

checkUpdateName :: RoomId -> T.Text -> IO ()
checkUpdateName roomId' newName = do
  getResp $ UpdateRoomName roomId' newName
  updatedNameRoom <- getResp $ GetRoom roomId'
  assertBool "updated name" (roomName updatedNameRoom == newName)

checkUpdateTopic :: RoomId -> T.Text -> IO ()
checkUpdateTopic roomId' newTopic = do
  getResp $ UpdateRoomTopic roomId' newTopic
  updatedTopicRoom <- getResp $ GetRoom roomId'
  assertBool "updated topic" (roomTopic updatedTopicRoom == newTopic)

checkRoomLock :: RoomId -> IO ()
checkRoomLock roomId' = do
  getResp $ LockRoom roomId'
  lockedRoom <- getResp $ GetRoom roomId'
  assertBool "room locked" (roomLocked lockedRoom == True)

checkRoomUnlock :: RoomId -> IO ()
checkRoomUnlock roomId' = do
  getResp $ UnlockRoom roomId'
  unlockedRoom <- getResp $ GetRoom roomId'
  assertBool "room unlocked" (roomLocked unlockedRoom == False)

checkMessageSend :: RoomId -> MessageType -> T.Text -> IO MessageId
checkMessageSend roomId' msgType body = do
  sentMsg <- getResp $ SendMessage roomId' msgType body
  msgs <- getResp $ GetMessages roomId' (Just 3) Nothing
  assertBool "sent message in past 3 messages"
    (messageId sentMsg `elem` map messageId msgs)
  return $ messageId sentMsg

-- assumes the message to be highlighted is in the past 10 messages
checkHighlightMessage :: RoomId -> MessageId -> IO ()
checkHighlightMessage roomId' messageId' = do
  getResp $ HighlightMessage messageId'
  msgs <- getResp $ GetMessages roomId' (Just 10) Nothing
  assertBool "highlighted message in past 10 messages" $
    case filter (\m -> messageId' == messageId m) msgs of
      [] -> False
      (m:_) -> messageStarred m

-- assumes the message to be highlighted is in the past 10 messages
checkUnhighlightMessage :: RoomId -> MessageId -> IO ()
checkUnhighlightMessage roomId' messageId' = do
  getResp $ UnhighlightMessage messageId'
  msgs <- getResp $ GetMessages roomId' (Just 10) Nothing
  assertBool "unhighlighted message in past 10 messages" $
    case filter (\m -> messageId' == messageId m) msgs of
      [] -> False
      (m:_) -> not $ messageStarred m

checkFileUpload :: RoomId -> FilePath -> T.Text -> IO ()
checkFileUpload roomId' fileName fileContents = do
  (tfp, h) <- liftM (\(a,b) -> (takeFileName a, b)) $ openTempFile "." fileName
  hPutStrLn h $ T.unpack fileContents
  hClose h
  getResp $ UploadFile roomId' tfp
  uploads <- getResp $ GetFileUploads roomId'
  case uploads of
    [] -> assertFailure "List of file uploads empty after uploading file"
    (u:_) -> do
      assertBool
        "check the file we uploaded has the same name as the first upload"
        ((T.pack tfp) == (uploadName u))
