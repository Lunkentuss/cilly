module GitlabSource (
  makeSource
) where

import Data.ByteString.Lazy (ByteString, pack)
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Data.Text hiding (elem, filter, map, findIndex,all)
import Data.List hiding (map,all)
import Data.Aeson.KeyMap (toList)
import Data.Aeson.Key (toString)
import Data.Yaml (
    encode
  , decode
  , FromJSON
  , ToJSON
  , parseJSON
  , withObject
  , Object
  , ParseException
  , decodeFileEither
  , (.:)
  , (.:?)
  , (.!=)
  )
import qualified Data.Yaml as Yaml
import Data.Aeson.Types (Key, Parser, Value)
import GHC.Generics (Generic)
import System.Exit

import qualified Source

isUnavailableJobName :: String -> Bool
isUnavailableJobName entry = entry `elem` unavailableJobNames where
  unavailableJobNames = [
    "image",
    "services",
    "stages",
    "types",
    "before_script",
    "after_script",
    "variables",
    "cache",
    "include"]

isValidJobName :: String -> Bool
isValidJobName entry = all ($ entry) [isValidName, not . isUnavailableJobName]
  where isValidName (x:_) = x /= '.'
        isValidName "" = False

defaultStage :: String
defaultStage = "test"

defaultStages :: [String]
defaultStages = ["build", "test", "deploy"]

makeSource :: Source.Source
makeSource = Source.Source {
  name = "Gitlab",
  isValidPipeline = isValidPipeline,
  makePipeline = makePipeline
  }

makePipeline :: Source.Path -> IO Source.Pipeline
makePipeline root = do
  objectEither <- decodeFileEither $ root ++ ".gitlab-ci.yml"
    :: IO (Either ParseException Pipeline)
  object <- case objectEither of
    Right x -> return x
    Left err -> exit $ show err
  return $ makePipelineNoIO object

-- Checks if a gitlab source is available.
-- Return always for now.
isValidPipeline :: Source.Path -> IO Bool
isValidPipeline root = return True

makePipelineNoIO Pipeline {..} =
  let
    allStages = stages
      & (if ".pre" `elem` stages then id else ([".pre"] <>))
      & (if ".post" `elem` stages then id else (<> [".post"]))
  in
    Source.Pipeline {
        jobs = makeJobs allStages variables jobs
      , stages = allStages
    }

makeJobs stages variables jobs_ =
  let nonEmptyStages = stages
        & filter (`elem` map stage jobs_)
      stageToJobs = nonEmptyStages
        & map (\stage_ -> (stage_, filter ((==stage_) . stage) jobs_))
        & Map.fromList
      prevNonEmptyStage stage_ = nonEmptyStages
        & elemIndex stage_
        & (subtract 1 <$>)
        & (>>= (\x -> if x < 0 then Nothing else Just x))
        & ((nonEmptyStages!!)<$>)
      makeJob_ job = makeJob
        job
        (stage job
          & prevNonEmptyStage
          & maybe [] (stageToJobs Map.!)
          & map name
        )
        variables
  in map makeJob_ jobs_

makeJob (Job {..}) jobDependencies pipelineVariables = Source.Job {
    artifacts = makeArtifacts artifacts
  , jobDependencies = jobDependencies
  , scripts = [
        Source.Script (beforeScript <> script) allowFailure
    ] <> maybe [] (\script -> [Source.Script script True]) afterScript
  , variables = variables <> pipelineVariables
  , ..
  }
makeArtifacts Artifacts {..} = Source.Artifacts {..}

parseKeyMap
  :: Object
  -> (Key -> Bool)
  -> (Key -> Value -> Parser a)
  -> Parser [a]
parseKeyMap object filterf parser =
  toList object
    & filter (\(key, _) -> filterf key)
    & traverse (uncurry parser)

data Pipeline = Pipeline {
  stages :: [String],
  jobs :: [Job],
  variables :: Map.Map String String
} deriving (Show)

data Job = Job {
    name :: String
  , image :: Maybe String
  , entrypoint :: Maybe [String]
  , stage :: String
  , artifacts :: Artifacts
  , beforeScript :: [String]
  , script :: [String]
  , afterScript :: Maybe [String]
  , allowFailure :: Bool
  , variables :: Map.Map String String
} deriving (Show)

newtype Artifacts = Artifacts {
  paths :: [String]
} deriving (Show, Generic)

instance FromJSON Artifacts

instance FromJSON Pipeline where
  parseJSON = withObject "Pipeline" $ \v -> Pipeline
    <$> v .:? "stages" .!= defaultStages
    <*> parseKeyMap v (isValidJobName . toString) (parseJob . toString)
    <*> v .:? "variables" .!= Map.empty

parseJob :: String -> Value -> Parser Job
parseJob name = withObject "Job" $ \o -> do
  imageValue <- o .:? "image"
  (image, entrypoint) <- case imageValue of
    Just imageValue' -> case imageValue' of
      Yaml.Object imageObject -> do
        image <- imageObject .:? "name"
        entrypoint <- imageObject .:? "entrypoint"
        return (image, entrypoint)
      Yaml.String image -> return (Just $ unpack image, Nothing)
    Nothing -> return (Nothing, Nothing)
  stage <- o .:? "stage" .!= defaultStage
  artifacts <- o .:? "artifacts" .!= Artifacts []
  beforeScript <- o .:? "before_script" .!= []
  script <- o .: "script"
  afterScript <- o .:? "after_script" .!= Nothing
  allowFailure <- o .:? "allow_failure" .!= False
  variables <- o .:? "variables" .!= Map.empty
  return $ Job {name = name, ..}

exit err = do
  putStrLn err
  exitWith (ExitFailure 1)
