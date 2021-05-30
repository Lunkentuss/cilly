module CliController (
    controller
) where

import Control.Monad
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe
import System.Exit (ExitCode(..))

import Concurrent
import JobDispatcher
import qualified Source

data ControllerState = ControllerState {
    continue :: Bool
  , outputCache :: Map.Map String (String, Bool)
  , focusedJob :: Maybe String
  , success :: Bool
}

controller
  :: Source.Pipeline
  -> TMessages DispatcherJobMessage
  -> IO ExitCode
controller _ messages = do
  let bold str = "\ESC[1m" <> str <> "\ESC[0m"
  let printJobHeader jobId = do
        putStrLn $ bold $ "[Running job: " <> jobId <> "]"
  let flushJob jobId state = do
        printJobHeader jobId
        let jobCache = outputCache state Map.! jobId
        jobCache & fst & putStr
        return state { outputCache = outputCache state & Map.delete jobId }
  let flushFinishedJobs state = do
        let finishedJobs = Map.filter snd (state & outputCache)
        let unfinishedJobs = Map.filter (not . snd) (state & outputCache)
        mapM_ (flip flushJob state . fst) (Map.toAscList finishedJobs)
        return state { outputCache = unfinishedJobs }
  let isFocusedJob jobId state =
        Just jobId == focusedJob state
  let initCache jobId state =
        outputCache state
        & Map.insert jobId ("", False)
        & \newOutputCache -> state { outputCache = newOutputCache }
  let initCaches jobIds state = foldr initCache state jobIds
  let updateFocused newFocusedJob state = state { focusedJob = newFocusedJob }
  let setFinished jobId state = state {
    outputCache =
      outputCache state
      & Map.adjust (\(cache, _) -> (cache, True)) jobId
  }
  let appendCache jobId appendCache state = state {
    outputCache =
      outputCache state
      & Map.adjust (\(cache, bool) -> (cache <> appendCache, bool)) jobId
  }
  let appendLog jobId message prevState = do
        if isFocusedJob jobId prevState
          then do
            putStr message
            return prevState
          else do
            return $ appendCache jobId message prevState
  let setFail state = state { success = False }
  let continue' = continue
  let outputCache' = outputCache
  let finishJob jobId prevState = do
        if isFocusedJob jobId prevState
          then do
            flushedState <- flushFinishedJobs prevState
            if Map.size (outputCache' flushedState) == 0
              then return flushedState { focusedJob = Nothing }
              else do
                let newFocusedJob =
                      Map.keys (outputCache' flushedState)
                      & head
                newState <- flushJob newFocusedJob flushedState
                return newState { focusedJob = Just newFocusedJob }
          else do
            return $ setFinished jobId prevState
  let printLoop prevState@ControllerState {..} = do
        msg <- popIO messages
        newState <- case msg of
          DJobFinished {..} -> finishJob jobId prevState
          DJobOutput {..} -> appendLog jobId message prevState
          DJobsStarted {..} -> do
            case focusedJob of
              Just job -> do
                prevState
                  & initCaches jobIds
                  & return
              Nothing -> do
                let newFocusedJob = head jobIds
                printJobHeader newFocusedJob
                prevState
                  & updateFocused (Just newFocusedJob)
                  & initCaches (tail jobIds)
                  & return
          DJobFailed {..} -> do
            state <- appendLog jobId (message <> "\n") prevState
            setFail <$> finishJob jobId state
          DAllJobsFinished -> do
            return prevState { continue = False }
        if continue' newState
          then printLoop newState
          else return newState
  finalState <- printLoop ControllerState {
      continue = True
    , outputCache = Map.empty
    , focusedJob = Nothing
    , success = True
  }
  return $ if success finalState then ExitSuccess else ExitFailure 1
