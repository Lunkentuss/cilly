module ExtractArtifacts (
  extractArtifacts
) where

import Path.IO (copyDirRecur)
import Path.Posix (parseAbsDir)

import ExceptionWrapper (wrapException)

extractArtifacts sourceDir targetDir = go
  `wrapException`
  (
    "Failed to extract artifacts by recursively copying directory '"
    <> sourceDir
    <> "' into directory '"
    <> targetDir
    <> "'"
  )
  where
  go = do
    sourceDirPath <- parseAbsDir sourceDir
    targetDirPath <- parseAbsDir targetDir
    copyDirRecur sourceDirPath targetDirPath
