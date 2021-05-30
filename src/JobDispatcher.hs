module JobDispatcher(
    DispatcherJobMessage(..)
  , runJobDispatcher
) where

import Control.Concurrent
import Control.Monad
import Data.Function ((&))
import qualified Data.List as List

import Concurrent
import qualified Graph
import JobMessage
import qualified Source

data DispatcherJobMessage =
    DJobFinished {
        jobId :: String
    }
  | DJobOutput {
        jobId :: String
      , message :: String
    }
  | DJobsStarted {
        jobIds :: [String]
    }
  | DJobFailed {
        jobId :: String
      , message :: String
    }
  | DAllJobsFinished

data RunState = RunState {
    runningJobs :: [String]
  , jobGraph :: Graph.Graph String
  , failedJobs :: Bool
  , finishedJobs :: [String]
} deriving Show

runJobDispatcher
  :: TMessages DispatcherJobMessage
  -> Graph.Graph String
  -> Int
  -> (TMessages JobMessage -> String -> [String] -> IO ())
  -> IO ()
runJobDispatcher messages initJobGraph maxConcurrentJobs runJob = do
  jobMessages <- newTMessagesIO
  let sendMessage = appendIO messages
  let runningJobsCount = length . runningJobs
  let launchJobs prevState@RunState {..} = do
        let launchingJobs =
              take
                (maxConcurrentJobs - length runningJobs)
                (Graph.sourceNodes jobGraph
                  & filter (not . flip elem runningJobs)
                )
        forM_ launchingJobs $ \job -> do
              forkIO $ runJob jobMessages job finishedJobs
        unless (null launchingJobs)
          (sendMessage $ DJobsStarted launchingJobs)
        return prevState {
            runningJobs = runningJobs <> launchingJobs
        }
  let finalizeJob jobId prevState@RunState {..} = prevState {
      runningJobs = List.delete jobId runningJobs
    , jobGraph = Graph.removeNode jobId jobGraph
    , finishedJobs = finishedJobs <> [jobId]
  }
  let setFailedJobs prevState = prevState { failedJobs = True }
  let continueLoop prevState@RunState {..} =
        runningJobsCount prevState == 0
        || (failedJobs && null runningJobs)

  let runLoop prevState = do
        msg <- popIO jobMessages
        newState <- case msg of
          JobFinished {..} -> do
            sendMessage DJobFinished {..}
            let state = finalizeJob jobId prevState
            if failedJobs state then return state else launchJobs state
          JobOutput {..} -> do
            sendMessage DJobOutput {..}
            return prevState
          JobFailed {..} -> do
            sendMessage DJobFailed {
                jobId = jobId
              , message = "Job returned a non zero exit code: " <> show exitCode
              }
            finalizeJob jobId prevState
                & setFailedJobs
                & return
          JobError {..} -> do
            sendMessage DJobFailed {..}
            finalizeJob jobId prevState
                & setFailedJobs
                & return
        if continueLoop newState
          then do
            sendMessage DAllJobsFinished
            return ()
          else runLoop newState

  initState <- launchJobs $ RunState {
      runningJobs = []
    , jobGraph =  initJobGraph
    , failedJobs = False
    , finishedJobs = []
  }
  runLoop initState
