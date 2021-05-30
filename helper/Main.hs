module Main where

import CliOptions (parseCliOptions, CliOptions(..))
import ArchiveArtifacts
import ExtractArtifacts

main = do
  options <- parseCliOptions
  case options of
    ArchiveArtifacts {..} -> do
      archiveArtifacts sourceDir targetDir paths
    ExtractArtifacts {..} -> do
      extractArtifacts sourceDir targetDir
