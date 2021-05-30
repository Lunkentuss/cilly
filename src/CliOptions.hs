module CliOptions (
    CliOptions(..)
  , parseCliOptions
) where

import Options.Applicative

data CliOptions = CliOptions {
    ui :: String
  , jobs :: Int
}

config :: Parser CliOptions
config = CliOptions
  <$> strOption
      ( long "ui"
     <> short 'u'
     <> help "The user interface type, is either cli or tui"
     <> showDefault
     <> value "cli")
  <*> option auto
      ( long "jobs"
     <> short 'j'
     <> help "Run the specified number of jobs in parallel"
     <> showDefault
     <> value 5
     <> metavar "INT" )

opts :: ParserInfo CliOptions
opts = info (config <**> helper)
  ( fullDesc
  <> progDesc "Run Gitlab CI jobs locally"
  <> header "cilly")

parseCliOptions = execParser opts
