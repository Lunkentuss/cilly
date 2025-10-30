module ContainerFacade (
    runContainerCommands
  , runContainerCommandsRecordLines
  , sanitizeContainerName
  , withVolume
) where

import Control.Exception (bracket)
import Control.Monad
import Data.Function ((&))
import Data.IORef
import qualified Data.Map as Map

import DockerTypes
import qualified Docker
import qualified Shell

-- Substitutes chracters which is not valid in a docker container
-- name.
-- Valid regex: [a-zA-Z0-9][a-zA-Z0-9_.-]*
sanitizeContainerName :: String -> String
sanitizeContainerName "" = ""
sanitizeContainerName (x:xs) =
  mapChar firstCharValid '0' x : map (mapChar restCharValid '_') xs
  where
    firstCharValid = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']
    restCharValid = firstCharValid <> "_.-"
    mapChar validCharSet sub char = if char `elem` validCharSet then char else sub

withVolume
  :: VolumeConf
  -> (Volume -> IO a)
  -> IO a
withVolume conf = bracket (Docker.createVolume conf) Docker.rmVolume

findShell :: String
findShell = [
  "if [ -x /usr/local/bin/bash ]; then",
  "exec /usr/local/bin/bash $@",
  "elif [ -x /usr/bin/bash ]; then",
  "exec /usr/bin/bash $@",
  "elif [ -x /bin/bash ]; then",
  "exec /bin/bash $@",
  "elif [ -x /usr/local/bin/sh ]; then",
  "exec /usr/local/bin/sh $@",
  "elif [ -x /usr/bin/sh ]; then",
  "exec /usr/bin/sh $@",
  "elif [ -x /bin/sh ]; then",
  "exec /bin/sh $@",
  "elif [ -x /busybox/sh ]; then",
  "exec /busybox/sh $@",
  "else",
  "echo shell not found",
  "exit 1",
  "fi"]
    & concatMap (\s -> [s, "\n"])
    & concat

createContainerConf
  :: String
  -> Maybe [String]
  -> [DockerTypes.Bind]
  -> ContainerConf
createContainerConf image entrypoint binds =
  containerConf {
      image = image
    , entrypoint = entrypoint
    , command = [ "sh", "-c", findShell ]
    , binds = binds
  }

-- Run a container. The logCallback is called when new input is read from the
-- stdout/stderr. The created container is removed on success.
runContainerAndCleanUp
  :: String
  -> Shell.ShellCommands
  -> Map.Map String String
  -> (String -> IO ())
  -> DockerTypes.ContainerConf
  -> IO DockerTypes.ContainerInfo
runContainerAndCleanUp containerName commands variables logCallback conf = do
  let image = conf & DockerTypes.image
  Docker.pullImage $ conf & DockerTypes.image
  bracket
    (Docker.runContainer containerName conf)
    Docker.rmContainer
    $ \container -> do
    Docker.attach container (Shell.commandsToShellScript commands variables) logCallback
    Docker.containerInfo container

runContainerCommands
  :: String
  -> (String -> IO ())
  -> String
  -> Maybe [String]
  -> Shell.ShellCommands
  -> Map.Map String String
  -> [DockerTypes.Bind]
  -> IO DockerTypes.ContainerInfo
runContainerCommands containerName logCallback image entrypoint commands variables binds =
  runContainerAndCleanUp containerName commands variables logCallback (createContainerConf image entrypoint binds)

runContainerCommandsRecordLines
  :: String
  -> String
  -> Maybe [String]
  -> Shell.ShellCommands
  -> Map.Map String String
  -> [DockerTypes.Bind]
  -> IO ([String], DockerTypes.ContainerInfo)
runContainerCommandsRecordLines containerName image entrypoint commands variables binds = do
  record <- newIORef ""
  dockerInfo <- runContainerAndCleanUp
    containerName
    commands
    variables
    (\output -> readIORef record >>= (\prev -> writeIORef record $ prev <> output))
    (createContainerConf image entrypoint binds)
  (,dockerInfo) . lines <$> readIORef record
