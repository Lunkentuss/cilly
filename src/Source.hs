module Source (
    Source(..)
  , Pipeline(..)
  , Job(..)
  , Artifacts(..)
  , Path
  , Script(..)
  , nameToJob
  , intrinsicJobDependencies
) where

import qualified Data.Map as Map
import qualified Graph
import Data.Function ((&))

type Path = String

data Source = Source {
    name :: String
  , isValidPipeline :: Path -> IO Bool
  , makePipeline :: Path -> IO Pipeline
}

data Pipeline = Pipeline {
    stages :: [String]
  , jobs :: [Job]
} deriving (Show)

data Script = Script {
    commands :: [String]
  , allowFailure :: Bool
} deriving (Show)

data Job = Job {
    name :: String
  , image :: Maybe String
  , entrypoint :: Maybe [String]
  , stage :: String
  , artifacts :: Artifacts
  , scripts :: [Script]
  , jobDependencies :: [String]
  , variables :: Map.Map String String
} deriving (Show)

instance Eq Job where
  (==) job1 job2 = job1.name == job2.name

intrinsicJobDependencyNames
  :: Job
  -> [Job]
  -> [String]
intrinsicJobDependencyNames job jobs =
  let
    jobGraph = Graph.createGraph' (.name) (.jobDependencies) (if job `elem` jobs then jobs else job : jobs)
  in
    Graph.downstreamNodes job.name jobGraph

-- Given a set of jobs and a a target job, this function returns all intrinsic
-- dependencies of the job. For example, the below graph with A, [B,C,D,E,F] as
-- input returns [B,C,D,F]. Note that E is not within the set.
--
--     D
--     ^
--     |
-- B<--|-->C-->F
--     |   ^
--     |   |
--     A   E
intrinsicJobDependencies
  :: Job
  -> [Job]
  -> [Job]
intrinsicJobDependencies job jobs =
  let
    allJobs = (if job `elem` jobs then jobs else job : jobs)
    nameToJob name = (map (\job -> (job.name, job)) allJobs & Map.fromList) Map.! name
  in
    intrinsicJobDependencyNames job allJobs
      & map nameToJob

newtype Artifacts = Artifacts {
  paths :: [String]
} deriving (Show)

nameToJob :: Pipeline -> String -> Job
nameToJob pipeline name_ =
  let map_ = jobs pipeline
        & map (\job -> (job.name, job))
        & Map.fromList
  in (map_ Map.! name_)
