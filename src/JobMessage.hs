module JobMessage(
  JobMessage(..)
) where

data JobMessage =
    JobFinished {
        jobId :: String
    }
  | JobOutput {
        jobId :: String
      , message :: String
    }
  -- If the return code of the script returned a non zero code
  | JobFailed {
        jobId :: String
      , exitCode :: Int
    }
  -- If the job couldn't be started or failed to close. For example, if the
  -- container image of the job failed to be pulled.
  | JobError {
        jobId :: String
      , message :: String
    }
