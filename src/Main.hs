module Main where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Data.Foldable
import Data.Function ((&))
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Directory (getCurrentDirectory)
import System.Exit
import System.Random

import CliOptions (CliOptions(..), parseCliOptions)
import Concurrent
import ContainerFacade
import DockerTypes
import qualified Git
import qualified GitlabSource
import qualified Graph
import qualified JobExecutor
import qualified JobDispatcher
import qualified Shell
import qualified Source
import qualified Docker
import qualified CliController
import qualified TuiController

cillyHelperImage :: String
cillyHelperImage = "lunkentuss/cilly-helper:0.2.0"

gitProjectDir :: String
gitProjectDir = "/git-project"

truncateGitCommitIdSize :: Int
truncateGitCommitIdSize = 8

sources :: [Source.Source]
sources = [GitlabSource.makeSource]

indent :: Int -> String -> String
indent n = (replicate n ' ' ++)

randomString :: IO String
randomString = take 20 . randomRs ('a', 'z') <$> newStdGen

assertOnlyOneValidResource :: [Source.Source] -> IO ()
assertOnlyOneValidResource validSources = do
  let validSourceNames = map @Source.Source @String (.name) validSources
  let allSourceNames = map @Source.Source @String (.name) validSources
  case length validSources of
    1 -> return ()
    0 -> do
      putStrLn $
        "Could not find a suitable CI source from any of the following "
        ++ "available resources:"
      traverse_ (putStrLn . indent 1) allSourceNames
      exitWith (ExitFailure 1)
    _ -> do
      putStrLn $
        "Found two or more sources: "
        ++ show validSourceNames
      exitWith (ExitFailure 1)

runPipelineJobs
  :: Source.Pipeline
  -> TMessages JobDispatcher.DispatcherJobMessage
  -> Int
  -> JobExecutor.JobConfig
  -> IO ()
runPipelineJobs pipeline messages maxConcurrentJobs jobConfig = do
  let nameToJob = Source.nameToJob pipeline
  let jobGraph =
        let
          jobToName job = job.name
        in
          Graph.transpose $ Graph.createGraph
            jobToName
            (map nameToJob . Source.jobDependencies)
            (Source.jobs pipeline)
  forkIO $ JobDispatcher.runJobDispatcher
    messages
    jobGraph
    maxConcurrentJobs
    (\messages job finishedJobs ->
      JobExecutor.runJob
        messages
        (nameToJob job)
        (map nameToJob finishedJobs)
        jobConfig
    )
  return ()

main :: IO ()
main = do
  options <- parseCliOptions
  controller <- case ui options of
    "cli" -> return CliController.controller
    "tui" -> return TuiController.controller
    uiOption -> do
      putStrLn $ "ERROR: Unrecognized ui type " <> uiOption
      exitWith (ExitFailure 1)
  root <- (<>"/") <$> getCurrentDirectory
  let containerNameTransform name =
        sanitizeContainerName . (("cilly" <> "-" <> name <> "-")<>)
        <$> randomString
        
  validSources <- filterM (`Source.isValidPipeline` root) sources
  assertOnlyOneValidResource validSources
  let source = head validSources
  pipeline <- Source.makePipeline source root
  let sortedJobsMaybe = Graph.dependencyLayerSort
        (.name)
        (map (Source.nameToJob pipeline) . Source.jobDependencies)
        (pipeline & Source.jobs)
        & (reverse<$>)
        & (concat<$>)
  sortedJobs <- case sortedJobsMaybe of
      Just x -> return x
      Nothing -> do
        putStrLn "ERROR: Found circular job dependencies"
        exitWith (ExitFailure 1)
  withVolume volumeConf $ \gitProjectVolume -> do
    containerName <- containerNameTransform "fetch-rev"
    (gitInfoLines,gitInfoContainerInfo) <- runContainerCommandsRecordLines
      containerName
      cillyHelperImage
      [
        Shell.ExecuteShell $ "cd " <> root
      , Shell.ExecuteShell   "git rev-parse HEAD"               -- commitID
      , Shell.ExecuteShell   "git rev-parse --abbrev-ref HEAD"  -- branchName
      ]
      Map.empty
      [
        bind {
            source = root
          , target = root
          , access = RO
        }
      ]
    case gitInfoContainerInfo & state & exitCode of
      0 -> return ()
      _ -> do
        putStrLn $ "Failed to fetch git information. Are you sure "
          <> root
          <> " is a git repository?"
        mapM_ print gitInfoLines
        exitWith (ExitFailure 1)
    let [commitId, branchName] = gitInfoLines
    putStrLn $
         "Checking out "
      <> branchName
      <> " as "
      <> take truncateGitCommitIdSize commitId

    initCloneContainerName <- containerNameTransform "init-clone"
    (cloneLines, cloneContainerInfo) <- runContainerCommandsRecordLines
      initCloneContainerName
      cillyHelperImage
      (Git.gitLocalCloneCommands root gitProjectDir Nothing)
      Map.empty
      [
        bind {
            source = root
          , target = root
          , access = RO
        },
        bind {
            source = gitProjectVolume.name
          , target = gitProjectDir
          , access = RW
        }
      ]
    case cloneContainerInfo & state & exitCode of
      0 -> return ()
      _ -> do
        putStrLn "Failed to clone the repository to docker volume."
        mapM_ print cloneLines
        exitWith (ExitFailure 1)

    messages <- newTMessagesIO
    withVolume volumeConf $ \artifactsVolume -> do
      let jobConfig = JobExecutor.JobConfig {
          gitOriginDir = root
        , gitOriginVolume = gitProjectVolume
        , artifactsVolume = artifactsVolume
        , cillyHelperImage = cillyHelperImage
        , ..
      }
      runPipelineJobs pipeline messages (jobs options) jobConfig
      exitCode <- controller pipeline messages
      exitWith exitCode
