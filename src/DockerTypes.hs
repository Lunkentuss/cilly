module DockerTypes (
  Container(..),
  ContainerConf(..),
  ContainerInfo(..),
  ContainerInfoState(..),
  Volume(..),
  VolumeConf(..),
  Bind(..),
  BindAccess(..),
  containerConf,
  volumeConf,
  bind,
) where

import Data.Function
import Data.List
import Data.Aeson.Types (
  FromJSON
  , ToJSON
  , parseJSON
  , toJSON
  , (.:)
  , object
  , (.=)
  , withObject)
import GHC.Generics (Generic)

data ContainerConf = ContainerConf {
    image :: String
  , entrypoint :: Maybe [String]
  , command :: [String]
  , tty :: Bool
  , attachStdin :: Bool
  , attachStdout :: Bool
  , attachStderr :: Bool
  , openStdin :: Bool
  , stdinOnce :: Bool
  , binds :: [Bind]
} deriving (Show)

newtype Container = Container {
  id_ :: String
} deriving (Show)

newtype ContainerInfo = ContainerInfo {
  state :: ContainerInfoState
} deriving (Show)

data ContainerInfoState = ContainerInfoState {
   exitCode :: Int
 , error :: String
} deriving (Show)

newtype HostConfig = HostConfig {
  binds :: [Bind]
} deriving (Show)

data Bind = Bind {
    source :: String
  , target :: String
  , access :: BindAccess
} deriving (Show)

data BindAccess = RO | RW deriving (Show)

-- Used when creating a volume
newtype VolumeConf = VolumeConf { name :: String }
-- Response object when creating a volume
newtype Volume = Volume { name :: String }

containerConf = ContainerConf {
    image = "unknown"
  , entrypoint = Nothing
  , command = ["/bin/sh"]
  , tty = False
  , attachStdin = True
  , attachStdout = True
  , attachStderr = True
  , openStdin = True
  , stdinOnce = True
  , binds = []
}

bind :: Bind
bind = Bind {
  source = "/",
  target = "/",
  access = RO
}

volumeConf = VolumeConf { name = "" }

instance ToJSON ContainerConf where
  toJSON ContainerConf {..} =
    object [
        "Image" .= image
      , "Entrypoint" .= entrypoint
      , "Cmd" .= command
      , "Tty" .= tty
      , "AttachStdin" .= attachStdin
      , "AttachStdout" .= attachStdout
      , "AttachStderr" .= attachStderr
      , "OpenStdin" .= openStdin
      , "StdinOnce" .= stdinOnce
      , "HostConfig" .= HostConfig { binds = binds }
    ]

instance ToJSON HostConfig where
  toJSON HostConfig {..} =
    object [
      "Binds" .= binds
    ]

instance ToJSON Bind where
  toJSON (Bind source target access) =
    [source, target, accessStr]
    & intercalate ":"
    & toJSON
    where
      accessStr = case access of
        RO -> "ro"
        RW -> "rw"

instance ToJSON VolumeConf where
  toJSON (VolumeConf name) =
    object [
      "Name" .= name
    ]

instance FromJSON Container where
  parseJSON = withObject "Container" $ \v -> Container
      <$> v .: "Id"

instance FromJSON ContainerInfo where
  parseJSON = withObject "ContainerInfo" $ \v -> ContainerInfo
      <$> v .: "State"

instance FromJSON ContainerInfoState where
  parseJSON = withObject "ContainerInfoState" $ \v -> ContainerInfoState
      <$> v .: "ExitCode"
      <*> v .: "Error"

instance FromJSON Volume where
  parseJSON = withObject "Volume" $ \v -> Volume
      <$> v .: "Name"
