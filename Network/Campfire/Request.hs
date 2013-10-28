module Network.Campfire.Request
       ( -- * Exports exposed to the user
         setBaseUrl
       , setApiToken
       , requestToResponse
       , Requestable
       , CompletedRequest(..)
         -- * Exports meant to remain internal
       , Requestable(..)
       , doRequest
       , noResponse
       , baseUrl
       , apiToken
       , myUserAgent
       , StdMethod(GET,PUT,POST,DELETE)
       ) where

import Data.Conduit(ResourceT)
import Network.HTTP.Conduit
import Network.HTTP.Types(StdMethod(GET,PUT,POST,DELETE), renderStdMethod)

import Data.Aeson
import Data.Aeson.Types(parseEither)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Control.Monad(liftM)
import Control.Monad.Trans(liftIO, MonadIO)
import System.IO.Unsafe
import Control.Concurrent.MVar
import Control.Exception

-- | Typeclass representing a request 'req' that can be transformed into
-- a response 'resp' by making an HTTP request and parsing the result.
class FromJSON resp => Requestable req resp | req -> resp where
  request :: Monad m => req -> IO (Request m)
  request req = mkRequest (httpMethod req) (reqBody req) (reqToUrl req)

  reqBody :: req -> L.ByteString
  reqBody _ = ""

  httpMethod :: req -> StdMethod
  httpMethod _ = GET

  parseResp  :: req -> L.ByteString -> Either String resp
  parseResp r s =
    eitherDecode s >>= parseEither (\(o :: Object) -> o .: (respObjName r))

  respObjName :: req -> T.Text
  respObjName _ = ""

  reqToUrl    :: req -> String

-- | Datatype representing the (possibly unsuccessful) result of
-- performing a request using 'requestToResponse'.
data CompletedRequest a
                          -- | Succesful request with response
                        = CompletedRequest a
                          -- | Exception caught from performing HTTP request
                        | HttpFailure HttpException
                          -- | Failure parsing JSON
                        | ParseFailure String
                        deriving Show

-- | Given 'Requestable' req and resp, executes the request by making
-- an HTTP request and parsing the response, catching 'HttpException'.
requestToResponse :: (MonadIO m, Requestable req resp) =>
                     req -> m (CompletedRequest resp)
requestToResponse req = do
  eUnparsedResp <- liftIO $ liftM Right (request req >>= doRequest)
                   `catch` (\(e :: HttpException) -> return $ Left e)
  case eUnparsedResp of
    Left e -> return $ HttpFailure e
    Right unparsedResp ->
      case parseResp req unparsedResp of
        Left e -> return $ ParseFailure e
        Right v -> return $ CompletedRequest v

mkRequest :: StdMethod -> L.ByteString -> String -> IO (Request m)
mkRequest method' body' urlSuff = do
  urlReq <- baseUrl >>= parseUrl . (++ urlSuff)
  let req = urlReq { method = renderStdMethod method'
                   , requestBody = RequestBodyLBS body'
                   , secure = True
                   , requestHeaders =
                     ("Content-Type", "application/json") :
                     ("User-Agent", myUserAgent) :
                     (requestHeaders urlReq)
                   }
  apiToken' <- apiToken
  return $ applyBasicAuth apiToken' "X" req

noResponse :: Requestable req () => req -> L.ByteString -> Either String ()
noResponse _ _ = Right ()

doRequest :: MonadIO m => Request (ResourceT IO) -> m L.ByteString
doRequest req = liftM responseBody $ liftIO $ withManager $ httpLbs req

-- | Set the base URL (subdomain) for subsequent requests.
setBaseUrl :: String -> IO ()
setBaseUrl url = takeMVar baseUrlRef >> putMVar baseUrlRef url

-- | Set the API Token for subsequent requests.
-- See <https://github.com/37signals/campfire-api#authentication>
-- for additional information.
setApiToken :: String -> IO ()
setApiToken tok = do
  takeMVar apiTokenRef
  putMVar apiTokenRef $ C8.pack tok

baseUrl :: IO String
baseUrl = readMVar baseUrlRef

apiToken :: IO S.ByteString
apiToken = readMVar apiTokenRef

apiTokenRef :: MVar S.ByteString
apiTokenRef = unsafePerformIO $ newMVar $ error "api token not set"

baseUrlRef :: MVar String
baseUrlRef = unsafePerformIO $ newMVar $ error "base url not set"

myUserAgent :: S.ByteString
myUserAgent = "Haskell campfire-lib (josh.mcgrath08@gmail.com)"
