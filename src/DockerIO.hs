module DockerIO (
    addQueryParam
  , DockerResponse(..)
  , Stream(..)
  , withDockerRequestStream
  , initRequest
  , path
  , post
  , delete
  , execRequest
  , jsonBody
  , lbsBody
  , decodeJson
  , status
  , statusCodeCheck
  , statusCodeCheckConfig
  , StatusCodeCheckConfig(..)
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Aeson.Types (
    FromJSON
  , ToJSON
  )
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import Data.List (intercalate)
import Network.HTTP.Types.Status (
    Status
  , statusIsSuccessful
  )
import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Client (
    Response
  , Request
  , requestBody
  , responseBody
  , method
  , responseStatus
  , requestHeaders
  , RequestBody (RequestBodyLBS)
  )
import SocketIO (
    socketRequest
  , withRequestUpgradeTCP
  , Stream(..)
  )

socketPath :: FilePath
socketPath = "/var/run/docker.sock"

apiVersion:: String
apiVersion = "v1.41"

fullPath path = "/" ++ apiVersion ++ path

data DockerResponse = DockerResponse {
    request :: DockerRequest
  , response :: Response LBS.ByteString
} deriving (Show)

newtype DockerRequestException = DockerRequestException {
  dockerResponse :: DockerResponse
} deriving (Show)
instance Exception DockerRequestException

withDockerRequestStream
  :: DockerRequest
  -> (Stream -> IO a)
  -> IO a
withDockerRequestStream requestData = do
  let (path, reqMod) = fromDockerRequest requestData
  withRequestUpgradeTCP socketPath path reqMod

data DockerRequest = DockerRequest {
    dataPath :: String
  , dataMethod :: SBS.ByteString
  , dataRequestHeaders :: RequestHeaders
  , dataRequestBody :: RequestBody
  , dataQueryParam :: [(String,String)]
} deriving (Show)

instance Show RequestBody where
  show _ = "Undefined Show"

-- Utility functions for creating requests, e.g.
-- initReq & post & jsonBody object & execRequest
initRequest :: DockerRequest
initRequest = DockerRequest "" "GET" [] "" []

path :: String -> DockerRequest -> DockerRequest
path path reqMod = reqMod { dataPath = path }

post :: DockerRequest -> DockerRequest
post reqMod = reqMod { dataMethod = "POST" }

delete :: DockerRequest -> DockerRequest
delete reqMod = reqMod { dataMethod = "DELETE" }

jsonBody :: (ToJSON a) => a -> DockerRequest -> DockerRequest
jsonBody body reqMod = reqMod {
      dataRequestBody = RequestBodyLBS $ Aeson.encode body
    , dataRequestHeaders = dataRequestHeaders reqMod ++ [
        ("Content-Type", "application/json")
      ]
  }

lbsBody :: LBS.ByteString -> DockerRequest -> DockerRequest
lbsBody body reqMod = reqMod {
      dataRequestBody = RequestBodyLBS body
  }

addQueryParam
  :: [(String,String)]
  -> DockerRequest
  -> DockerRequest
addQueryParam addParam reqMod@DockerRequest {..} = reqMod {
    dataQueryParam = dataQueryParam <> addParam
  }


fromDockerRequest :: DockerRequest -> (String, Request -> Request)
fromDockerRequest DockerRequest {..} =
  (
    fullPath $ setQueryParam dataQueryParam dataPath
  ,
    \req -> req {
        method = dataMethod
      , requestHeaders = requestHeaders req <> dataRequestHeaders
      , requestBody = dataRequestBody
    }
  )

execRequest :: DockerRequest -> IO DockerResponse
execRequest requestData@DockerRequest {..} = do
  let (path, reqMod) = fromDockerRequest requestData
  response <- socketRequest
    socketPath
    path
    reqMod
  return $ DockerResponse requestData response

setQueryParam :: [(String,String)] -> String -> String
setQueryParam param path =
  path
  ++ (if not (null param) then "?" else "")
  ++ intercalate "&" (map (\(k, v) -> k ++ "=" ++ v) param)

decodeJson
  :: (FromJSON a)
  => DockerResponse
  -> IO a
decodeJson = return . decode . responseBody . response

data StatusCodeCheckConfig e = StatusCodeCheckConfig {
    exceptionMap :: [(Status, e)]
  , allowedStatuses ::  [Status]
}

statusCodeCheckConfig :: StatusCodeCheckConfig e
statusCodeCheckConfig = StatusCodeCheckConfig [] []

statusCodeCheck
  :: (Exception e)
  => StatusCodeCheckConfig e
  -> DockerResponse
  -> IO DockerResponse
statusCodeCheck StatusCodeCheckConfig {..} dockerResponse = do
  let status = responseStatus $ response dockerResponse
  unless (statusIsSuccessful status || (status `elem` allowedStatuses)) $ do
    case status `lookup` exceptionMap of
      Just exception -> throwIO exception
      Nothing -> throwIO $ DockerRequestException dockerResponse
  return dockerResponse

status
  :: DockerResponse
  -> Status
status = responseStatus . response

decode
  :: (FromJSON a)
  => LBS.ByteString
  -> a
decode str = case Aeson.eitherDecode str of
  Right v -> v
  Left err -> error $ "ERROR parsing docker response: " <> UTF8.toString str
