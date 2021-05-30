module JobExecutor(
    JobConfig(..)
  , runJob
) where

import Control.Monad (unless, void)
import Data.Function ((&))
import Data.List (find, unwords, intercalate)
import qualified Data.Map as Map

import Concurrent
import ContainerFacade
import Control.Exception (bracket, catch, SomeException, displayException)
import DockerTypes
import qualified JobMessage as Job
import qualified Docker
import qualified Git
import qualified Shell
import qualified Source

data JobConfig = JobConfig {
    gitOriginDir :: String
  , gitOriginVolume :: Volume
  , commitId :: String
  , artifactsVolume :: Volume
  , cillyHelperImage :: String
  , containerNameTransform :: String -> IO String
}

runJob
  :: TMessages Job.JobMessage
  -> Source.Job
  -> [Source.Job]
  -> JobConfig
  -> IO ()
runJob messages job finishedJobs jobConfig = do
  let sendMessage = appendIO messages
  let jobName = job.name
  catch
    (runJobNoExceptionHandling messages job finishedJobs jobConfig)
    (\(e :: SomeException) ->
      appendIO messages $ Job.JobError jobName $ displayException e
    )

runJobNoExceptionHandling
  :: TMessages Job.JobMessage
  -> Source.Job
  -> [Source.Job]
  -> JobConfig
  -> IO ()
runJobNoExceptionHandling messages job finishedJobs JobConfig {..} = do
  let sendMessage = appendIO messages
  let jobName = job.name
  case Source.image job of
    Just jobImage -> do
      let projectOCIDir = "/builds/project-0"
      let artifactsOCIDir = "/builds/cilly/artifacts"
      let scriptToShellCommands script =
            script
            & Source.commands
            & concatMap (\step ->
                [
                    Shell.LogShell $ "$ " <> step
                  , Shell.ExecuteShell step
                ]
              )
            & ((Shell.ExecuteShellQuiet $ "cd " <> projectOCIDir):)

      let runScript script variables = do
            withVolume volumeConf $ \volume -> do
              let projectBind = bind {
                          source = volume.name
                        , target = projectOCIDir
                        , access = RW
                      }
              let projectBindRO = bind {
                          source = volume.name
                        , target = projectOCIDir
                        , access = RO
                      }
              let cloneBinds =
                    [
                        bind {
                            source = gitOriginVolume.name
                          , target = gitOriginDir
                          , access = RO
                        }
                      , projectBind
                    ]
              let artifactsBinds =
                    [
                      bind {
                          source = artifactsVolume.name
                        , target = artifactsOCIDir
                        , access = RW
                      },
                      projectBind
                    ]
              let helperContainerName prefix = containerNameTransform $ prefix <> "-" <> jobName
              cloneJobContainerName <- helperContainerName "clone-job"
              runContainerCommands
                cloneJobContainerName
                (\_ -> return ())
                cillyHelperImage
                (Git.gitLocalCloneCommands
                  gitOriginDir
                  projectOCIDir
                  (Just commitId)
                )
                Map.empty
                cloneBinds
              extractArtifactsJobContainerName <- helperContainerName "extract-artifacts"
              runContainerCommands
                extractArtifactsJobContainerName
                (\_ -> return ())
                cillyHelperImage
                (
                  -- TODO: Normalize name of job. Dont forget to update both
                  -- places.
                  finishedJobs
                  & Source.intrinsicJobDependencies job
                  & filter (not . null . Source.paths . Source.artifacts)
                  & map
                    (
                      \job -> Shell.ExecuteShell $
                         [
                             "cilly-helper"
                           , "extract-artifacts"
                           , "--source-dir"
                           , Shell.quote $ artifactsOCIDir <> "/" <> job.name
                           , "--target-dir"
                           , Shell.quote projectOCIDir
                         ]
                         & unwords
                    )
                )
                Map.empty
                [
                  bind {
                      source = artifactsVolume.name
                    , target = artifactsOCIDir
                    , access = RO
                  },
                  projectBind
                ]
              containerName <- containerNameTransform jobName
              containerInfo <- runContainerCommands
                containerName
                (sendMessage . Job.JobOutput jobName)
                jobImage
                (scriptToShellCommands script)
                variables
                [projectBind]
              archiveArtifactsJobContainerName <- helperContainerName "archive-artifacts"
              unless
                (null . Source.paths . Source.artifacts $ job)
                (void $ archiveArtifacts archiveArtifactsJobContainerName artifactsBinds)
              return containerInfo
              where
                archiveArtifacts containerName = runContainerCommands
                  containerName
                  (\_ -> return ())
                  cillyHelperImage
                  (
                    job
                    & (Source.paths . Source.artifacts)
                    & (
                        \paths -> Shell.ExecuteShell $
                           [
                               "cilly-helper"
                             , "archive-artifacts"
                             , "--source-dir"
                             , Shell.quote projectOCIDir
                             , "--target-dir"
                             , Shell.quote $ artifactsOCIDir <> "/" <> job.name
                             , "--paths"
                             , intercalate "," paths
                           ]
                           & unwords
                      )
                    & (: [])
                  )
                  Map.empty

      scriptInfos <- zip (Source.scripts job) <$> mapM (flip runScript $ Source.variables job) (Source.scripts job)
      let exitCodes =
            scriptInfos
            & filter (not . Source.allowFailure . fst)
            & map (exitCode . state . snd)
      let firstNonZero = exitCodes
            & find (/=0)
      sendMessage $ case firstNonZero of
        Nothing -> Job.JobFinished jobName
        Just nonZeroCode -> Job.JobFailed jobName nonZeroCode

    Nothing -> do
      putStrLn
        $ "Missing image, skipping job: " ++ jobName
