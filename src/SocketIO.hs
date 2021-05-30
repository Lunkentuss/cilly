module SocketIO (
    socketRequest
  , withRequestUpgradeTCP
  , Stream(..)
) where

import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import Network.HTTP.Client
import Network.HTTP.Client.Internal (
    StatusHeaders(..)
  , connectionRead
  , connectionWrite
  , getResponse
  , makeConnection
  , parseStatusHeaders
  , requestBuilder
  )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

import ExceptionWrapper

data Stream = Stream {
    readStream :: IO BS.ByteString
  , writeStream :: BS.ByteString -> IO ()
}

socketRequest
  :: FilePath
  -> String
  -> (Request -> Request)
  -> IO (Response LBS.ByteString)
socketRequest filePath path requestModifier = do
  (manager, request) <- constructRequest filePath path requestModifier
  httpLbs request manager

withRequestUpgradeTCP
  :: FilePath
  -> String
  -> (Request -> Request)
  -> (Stream -> IO a)
  -> IO a
withRequestUpgradeTCP filePath path requestModifier f = do
  (manager, request) <- constructRequest filePath path requestModifier
  withConnection request manager $ \conn -> do
    let timeout = Nothing
    cont <- requestBuilder request conn
    StatusHeaders s version hs <- parseStatusHeaders Nothing conn timeout cont
    f $ Stream {
        readStream = connectionRead conn
      , writeStream = connectionWrite conn
      }

constructRequest
  :: FilePath
  -> String
  -> (Request -> Request)
  -> IO (Manager, Request)
constructRequest filePath path requestModifier = do
  manager <- unixSocketManager filePath
  initReq <- parseRequest $ "http://localhost" ++ path
  let request = requestModifier initReq
  return (manager, request)

unixSocketManager :: FilePath -> IO Manager
unixSocketManager filePath = do
  let mSettings = defaultManagerSettings {
    managerRawConnection = return openUnixSocket
  }
  newManager mSettings
  where
    exec = do
          s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
          S.connect s (S.SockAddrUnix filePath)
          makeConnection (SBS.recv s 8096)
                         (SBS.sendAll s)
                         (S.close s)
    openUnixSocket _ _ _ =
      exec `wrapException` ("failed to connect to socket " <> filePath)
