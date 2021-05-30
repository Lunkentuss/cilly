module Docker (
    pullImage
  , runContainer
  , rmContainer
  , createVolume
  , rmVolume
  , attach
  , containerInfo
) where

import Control.Exception (Exception)
import Control.Monad (when, void)
import Data.Function ((&))
import Data.Functor ((<&>))

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.Lazy.Char8 (fromStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.String
import qualified Network.HTTP.Types.Status as HttpStatus

import DockerIO (
    addQueryParam
  , withDockerRequestStream
  , Stream(..)
  , initRequest
  , execRequest
  , path
  , post
  , lbsBody
  , delete
  , execRequest
  , decodeJson
  , jsonBody
  , DockerResponse
  , status
  , statusCodeCheckConfig
  , StatusCodeCheckConfig(..)
  )
import qualified DockerIO

import DockerTypes (
    Container(..)
  , ContainerConf(..)
  , ContainerInfo(..)
  , Volume(..)
  , VolumeConf(..)
  )

newtype DockerException =
  ImageNotFound { image :: String }

instance Show DockerException where
  show e = case e of
    ImageNotFound {..} -> "Failed to pull image " <> image

instance Exception DockerException

statusCodeCheck
  :: StatusCodeCheckConfig DockerException
  -> DockerResponse
  -> IO DockerResponse
statusCodeCheck = DockerIO.statusCodeCheck

pullImage :: String -> IO ()
pullImage image = do
  status' <- initRequest
    & path ("/images/" <> image <> "/json")
    & execRequest
    >>= statusCodeCheck statusCodeCheckConfig {
      allowedStatuses = [
        HttpStatus.status404
      ]
    }
    <&> status
  when (status' == HttpStatus.status404) (void $ pullImage' image)

pullImage' :: String -> IO DockerResponse
pullImage' image = initRequest
  & post
  & path "/images/create"
  & addQueryParam [("fromImage", image)]
  & execRequest
  >>= statusCodeCheck statusCodeCheckConfig {
    exceptionMap = [
      (
          HttpStatus.status404
        , ImageNotFound image
      )
    ]
  }

runContainer
  :: String
  -> ContainerConf
  -> IO Container
runContainer name containerConf = do
  container <- initRequest
    & post
    & path "/containers/create"
    & addQueryParam [("name", name)]
    & jsonBody containerConf
    & execRequest
    >>= statusCodeCheck statusCodeCheckConfig
    >>= decodeJson
  initRequest
    & post
    & path ("/containers/" ++ id_ container ++ "/start")
    & execRequest
    >>= statusCodeCheck statusCodeCheckConfig
  return container

containerInfo
  :: Container
  -> IO ContainerInfo
containerInfo container = do
  initRequest
    & path ("/containers/" ++ id_ container ++ "/json")
    & execRequest
    >>= statusCodeCheck statusCodeCheckConfig
    >>= decodeJson

rmContainer :: Container -> IO ()
rmContainer container = do
  initRequest
    & post
    & path ("/containers/" ++ id_ container ++ "/stop")
    & execRequest
    >>= statusCodeCheck statusCodeCheckConfig {
      allowedStatuses = [HttpStatus.status304]
    }
  initRequest
    & delete
    & path ("/containers/" ++ id_ container)
    & execRequest
    >>= statusCodeCheck statusCodeCheckConfig
  return ()

createVolume
  :: VolumeConf
  -> IO Volume
createVolume volumeConf = do
  initRequest
    & post
    & path "/volumes/create"
    & jsonBody volumeConf
    & execRequest
    >>= statusCodeCheck statusCodeCheckConfig
    >>= decodeJson

rmVolume :: Volume -> IO ()
rmVolume volume = do
  initRequest
    & delete
    & path ("/volumes/" ++ volume.name)
    & execRequest
    >>= statusCodeCheck statusCodeCheckConfig
  return ()

attach
  :: Container
  -> String
  -> (String -> IO ())
  -> IO ()
attach container stdin callback = do
  withDockerRequestStream
    (initRequest
      & post
      & path ("/containers/" ++ id_ container ++ "/attach")
      & addQueryParam [
          ("stdout", "1")
        , ("stderr", "1")
        , ("stdin", "1")
        , ("logs", "1")
        , ("stream", "1")
        ]
    )
    $ \stream -> do
        writeStream stream $ fromString stdin
        parseDockerAttachFormat callback (readStream stream)

parseDockerAttachFormat
  :: (String -> IO ())
  -> IO BS.ByteString
  -> IO ()
parseDockerAttachFormat callback readBytes = go ""
  where
  go prevData = do
    data_ <- (prevData<>) . fromStrict <$> readBytes
    if LBS.length data_ == 0
      then return ()
      else readAllAvailableMessages data_ >>= go

  -- Calls the callback for all messages that are complete, any data
  -- corresponding to incomplete messages are returned.
  readAllAvailableMessages :: LBS.ByteString -> IO LBS.ByteString
  readAllAvailableMessages data_ = do
    case payloadLength data_ of
      Just payloadLength_ -> do
        let msgLength = payloadLength_ + headerLength
        if bllength data_ < msgLength
          then do
            return data_
          else do
            data_
              & bldrop headerLength
              & bltake payloadLength_
              & toString
              & callback
            readAllAvailableMessages $ bldrop msgLength data_
      Nothing -> return data_

  payloadLength :: LBS.ByteString -> Maybe Int
  payloadLength data_ = if bllength data_ < headerLength
    then Nothing
    else
      Just $ sum(
        map byteValue [0..(headerLengthByteSize-1)]
      )
    where
      byteValue byteIndex =
        (2 ^ (8 * (headerLengthByteSize - 1 - byteIndex)))
        * blindex data_ (byteIndex + headerLengthByteOffset)

  headerLength = 8
  headerLengthByteOffset = 4
  headerLengthByteSize = 4

  blindex :: LBS.ByteString -> Int -> Int
  blindex data_ index = LBS.index data_ (fromIntegral index)
    & (fromInteger . toInteger)

  bldrop :: Int -> LBS.ByteString -> LBS.ByteString
  bldrop count = LBS.drop (fromIntegral count)

  bltake :: Int -> LBS.ByteString -> LBS.ByteString
  bltake count = LBS.take (fromIntegral count)

  bllength :: LBS.ByteString -> Int
  bllength = fromInteger . toInteger . LBS.length
