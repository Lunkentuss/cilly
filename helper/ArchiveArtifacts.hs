module ArchiveArtifacts (
  archiveArtifacts
) where

import Control.Exception (Exception, throwIO)
import Path.IO (copyDirRecur, copyFile, ensureDir)
import Path.Posix (parent, parseAbsDir, parseRelDir, parseRelFile, toFilePath, (</>))
import System.Posix.Files (FileStatus, getFileStatus, isDirectory, isRegularFile)

import ExceptionWrapper (wrapException)

data BadFileTypeException = BadFileTypeException

instance Show BadFileTypeException where
  show BadFileTypeException = "expected either a regular file or a directory"

instance Exception BadFileTypeException

data FileType = RegularFile | Dir | NotValid deriving Show

fileType
  :: FileStatus
  -> FileType
fileType fileStatus
  | isRegularFile fileStatus = RegularFile
  | isDirectory fileStatus = Dir
  | otherwise = NotValid

archivePath
  :: String
  -> String
  -> String
  -> IO ()
archivePath sourceDir targetDir path = go
  `wrapException` ("Failed to archive path '" <> path <> "'")
  where
  go = do
    let fullSourceFilePath = sourceDir <> "/" <> path
    fileStatus <- getFileStatus fullSourceFilePath
    case fileType fileStatus of
      RegularFile -> do
        fullSourcePath <- (</>) <$> parseAbsDir sourceDir <*> parseRelFile path
        fullTargetPath <- (</>) <$> parseAbsDir targetDir <*> parseRelFile path
        ensureDir $ parent fullTargetPath
        copyFile fullSourcePath fullTargetPath
      Dir -> do
        fullSourcePath <- (</>) <$> parseAbsDir sourceDir <*> parseRelDir path
        fullTargetPath <- (</>) <$> parseAbsDir targetDir <*> parseRelDir path
        ensureDir $ parent fullTargetPath
        copyDirRecur fullSourcePath fullTargetPath
      NotValid -> do
        throwIO BadFileTypeException

archiveArtifacts sourceDir targetDir paths = do
  mapM_ (archivePath sourceDir targetDir) paths
