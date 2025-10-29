import System.Process (callProcess, callCommand)
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory, listDirectory)

relPathCilly = "/result/bin/cilly"

data Cmd = Gen | Check deriving (Show)

-- This program executes the snapshot testing. It can be executed with the
-- following commands:
--
-- check: Generates the current results of the input and compares it with the
--        last saved snapshots.
-- gen: Substitutes the saved snapshots with the current ones
main = do
  args <- getArgs
  rootDir <- getCurrentDirectory

  let cmd = case args of
        ["check"] -> Check
        ["gen"]   -> Gen
        []        -> Check
  let snapshotDir = rootDir ++ "/test_snapshot"  :: FilePath
  let outputDir = snapshotDir ++ "/output"  :: FilePath
  let outputDirTmp = snapshotDir ++ "/output_tmp"  :: FilePath
  let inputDir = snapshotDir ++ "/input"  :: FilePath
  inputFiles <- map ((inputDir ++ "/")++) <$> listDirectory inputDir
  let genOutputDir dir =
        mapM_
          (
            executeTest
            (rootDir ++ relPathCilly :: FilePath)
            dir
          )
          inputFiles
  case cmd of
    Gen -> do
      genOutputDir outputDir
    Check -> do
      callCommand $ "mkdir -p " ++ outputDirTmp
      genOutputDir outputDirTmp
      callCommand $ "diff -u " ++ outputDir ++ " " ++ outputDirTmp

executeTest
  :: FilePath
  -> FilePath
  -> FilePath
  -> IO ()
executeTest cillyBin outputDir gitlabCiFile = do
  putStrLn $ "Generating output for: " ++ gitlabCiFile
  callCommand $
    "bash -c '"
    ++ "    tmpdir=$(mktemp -d -t XXXXXX.cilly-snapshot-test)"
    ++ " && export GIT_AUTHOR_DATE='2000-01-01T00:00:00Z'"
    ++ " && export GIT_COMMITTER_DATE='2000-01-01T00:00:00Z'"
    ++ " && cd $tmpdir"
    ++ " && git init &> /dev/null"
    ++ " && cp " ++ gitlabCiFile ++ " .gitlab-ci.yml"
    ++ " && git add .gitlab-ci.yml"
    ++ " && git -c 'user.name=foo' -c user.email='foo@bar.com' commit -am 'tmp' &> /dev/null"
    ++ " && " ++ cillyBin ++ " > "
        ++ outputDir
        ++ "/$(basename "
          ++ gitlabCiFile
          ++ " | sed -E \"s/gitlab-ci-([^.]*)[.]yml/\\1/\")"
    ++ "'"
