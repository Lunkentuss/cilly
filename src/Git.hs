module Git (
    gitLocalCloneCommands
) where

import Data.Maybe

import qualified Shell

-- Shell commands for cloning a repository from a local
-- directory to a target directory. If commitId is not
-- Nothing, the commit will be checked out.
gitLocalCloneCommands
  :: String
  -> String
  -> Maybe String
  -> Shell.ShellCommands
gitLocalCloneCommands sourceDir targetDir commitId =
  [
    Shell.ExecuteShell $ "mkdir -p " <> targetDir
  , Shell.ExecuteShell $ "cd " <> targetDir
  , Shell.ExecuteShell   "git init"
  , Shell.ExecuteShell $ "git remote add origin " <> sourceDir
  , Shell.ExecuteShell $ "git remote set-url origin " <> sourceDir
  , Shell.ExecuteShell   "git fetch origin"
  , Shell.ExecuteShell $ "git checkout " <> fromMaybe "$(git remote show origin | sed -n '/HEAD branch/s/.*:[ ]*//p')" commitId
  , Shell.ExecuteShell   "cd -"
  ]
