module Network.Campfire.Upload where

import Network.Campfire.Common

import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData

-- | Request type for getting list of most recent uploads to 'RoomId'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/uploads.md#get-uploads>
-- for additional information.
data GetFileUploads = GetFileUploads RoomId
                    deriving (Show, Eq)

-- | Request type for getting upload in specified 'RoomId' with
-- specified 'MessageId'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/uploads.md#get-upload>
-- for additional information.
data GetFileUpload = GetFileUpload RoomId MessageId
                   deriving (Show, Eq)

-- | Request type for uploading a file with given 'FilePath' to
-- 'RoomId'. See
-- <https://github.com/37signals/campfire-api/blob/master/sections/uploads.md#create-upload>
-- for additional information.
data UploadFile = UploadFile RoomId FilePath
                deriving (Show, Eq)

instance Requestable GetFileUploads [Upload] where
  respObjName _ = "uploads"
  reqToUrl (GetFileUploads rId) = "/room/" ++ show rId ++ "/uploads.json"

instance Requestable GetFileUpload Upload where
  respObjName _ = "upload"
  reqToUrl (GetFileUpload rId mId) =
    "/room/" ++ show rId ++ "/messages/" ++ show mId ++ "/upload.json"

instance Requestable UploadFile () where
  parseResp = noResponse
  reqToUrl (UploadFile rId _) = "/room/" ++  show rId ++ "/uploads.json"

  request (UploadFile rId fPath) = do
    urlReq <- baseUrl >>= parseUrl . (++ urlSuff)
    formReq <- formDataBody [partFile "upload" fPath] urlReq
    let req = formReq { secure = True
                      , requestHeaders =
                        ("User-Agent", myUserAgent) :
                        (requestHeaders formReq)
                      }
    apiToken' <- apiToken
    return $ applyBasicAuth apiToken' "X" req

    where
      urlSuff = reqToUrl (UploadFile rId fPath)
