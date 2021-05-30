module CliOptions (
    CliOptions(..)
  , parseCliOptions
) where

import Options.Applicative
import Data.List.Split

data CliOptions =
  ArchiveArtifacts {
      sourceDir :: String
    , targetDir :: String
    , paths :: [String]
  } |
  ExtractArtifacts {
      sourceDir :: String
    , targetDir :: String
  }

archiveArtifactsParser = ArchiveArtifacts
  <$> strOption
      ( long "source-dir"
     <> help "The source directory used to archive artifacts from")
  <*> strOption
      ( long "target-dir"
     <> help "The target directory used to archive artifacts into")
  <*> (splitOn "," <$> strOption
      ( long "paths"
     <> help "Paths to archive, seperated by a comma (,)"))

extractArtifactsParser = ExtractArtifacts
  <$> strOption
      ( long "source-dir"
     <> help "The source directory used to extract artifacts from")
  <*> strOption
      ( long "target-dir"
     <> help "The target directory used to extract artifacts to")

config = hsubparser $
    command
      "archive-artifacts"
      (info archiveArtifactsParser ( progDesc "Archive artifacts into a directory" ))
 <> command
      "extract-artifacts"
      (info extractArtifactsParser ( progDesc "Extract artifacts into a directory" ))

opts :: ParserInfo CliOptions
opts = info (config <**> helper)
  ( fullDesc
  <> progDesc "Internal cilly tool"
  <> header "cilly-helper")

parseCliOptions = execParser opts
